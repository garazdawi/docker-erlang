# Using Minikube and Erlang

This is a quick demo of using minikube to run a distributed Erlang application.
The example we will use is the
[Docker Watch](http://github.com/erlang/docker-erlang-example/tree/master)
node as a base.

This is only meant to be an example of how to get started. It is not the only,
nor neccesarily the best way to setup minikube with distributed Erlang.

# Prerequisites

To start with you should familiarize yourself with minikube through this guide:
https://kubernetes.io/docs/setup/minikube/

In a nutshell:

## Install

 * [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
 * [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)
 * [minikube](https://github.com/kubernetes/minikube/releases)

## Start and test

    > minikube start
    > kubectl run hello-minikube --image=k8s.gcr.io/echoserver:1.10 --port=8080
    > kubectl expose deployment hello-minikube --type=NodePort
    > curl $(minikube service hello-minikube --url)
    ## Should print a lot of text
    > kubectl delete services hello-minikube
    > kubectl delete deployment hello-minikube
    > minikube stop

# Deploying Dockerwatch

In this demo we will be doing three things:

* Create a new application called dw-db (dockerwatch database) that uses mnesia
* Modify dockerwatch to use dw-db
* Create a Service for erlang distribution and a StatefullSet for the dw-db
* Create a Deployment of dockerwatch that implements the http/https Service

First however, make sure that the minikube cluster is started:

    > minikube start

and that you have cloned this repo and checked out this branch:

    > git clone https://github.com/erlang/docker-erlang-example
    > cd docker-erlang-example/advanced_examples/minikube-dist

## Create the dw-db backend

The purpose of the backend is to be the service responsible for writing
keeping the data. So the only thing it needs to do is some mnesia
initialization when [starting](backend/src/dockerwatch.erl).

Since we are running inside a kluster where each pod gets its own IP address
we don't really need epmd any more. So in this example we use the `-epmd_module`
to implement our [own name resolution](backend/src/epmd_dns_srv.erl)
based on EPMD and [DNS SRV](https://github.com/kubernetes/dns/blob/master/docs/specification.md#242---srv-records). 

The module is then configured to be used in [vm.args.src](backend/config/vm.args.src):

    -start_epmd false
    -epmd_module epmd_dns_srv

Lastly net_kernel has to be configured to listen to the port, this is done in the
[sys.config.src](backend/config/sys.config.src):

```
{kernel, [{inet_dist_listen_min, 12345}]},
```

## Modify dockerwatch

The modification of the dockerwatch module to use mnesia is pretty straight forward.
You can see the end result [here](dockerwatch/src/dockerwatch.erl).

The same modifications with distribution port has to be done for dockerwatch as well.
As we want to be able to scale the number of dockerwatch frontends as load increases
we need to use a node name that is unique in the cluster. Using the IP address of
the pod is a simple solution that works for now.

In order to get the IP the following config has to be added to the dockerwatch deployment
config:

```
      env:
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
```

and then we just use `${IP}` in [vm.args.src](dockerwatch/config/vm.args.src):

    -name dockerwatch@${IP}

We then use the DNS SRV record created by the dw-db service to get which node
we should connect to when doing a operations.

```
get_node_from_srv() ->
    {ok, SVC} = application:get_env(dockerwatch,backend),
    Nodes = inet_res:lookup(SVC,in,srv), %% Read the SRV record
    
    %% Calculate total Weight available
    NodeSum = lists:foldl(fun({_,Weight,_,_},Acc) ->
                                  Weight + Acc
                          end,0,Nodes),
                          
    %% Select one of the nodes to handle the request
    Rand = rand:uniform(NodeSum),
    Host = lists:foldl(fun({_,Weight,_,Host},Acc) when (Acc - Weight) =< 0 ->
                               Host;
                          ({_,Weight,_,_Host},Acc) when is_integer(Acc) ->
                               Acc - Weight;
                          (_,Host) ->
                               Host
                       end,Rand,Nodes),

    %% Create and return the selected node name
    list_to_atom("dockerwatch@"++Host).
```

## Deploy the backend
    
We first setup the backend node as a service in order to easily find how to connect with it
from the dockerwatch nodes. This only works if there is only one backend node. If you
want to add more you need to use a more complex solution.

```
kubectl create service clusterip db  --clusterip="None" --tcp=12345:12345
service/backend created
```

The backend is build like this:

    eval $(minikube docker-env)
    docker build -t dw-db -f Dockerfile.backend .

and then deployed like this:

```
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: db
spec:
  replicas: 2 # by default is 1
  serviceName: "db" # has to match .spec.template.metadata.labels.app
  selector:
    matchLabels:
      app: db # has to match .spec.template.metadata.labels
  template:
    metadata:
      labels:
        app: db # has to match .spec.selector.matchLabels
    spec:
      containers:
      - name: db
        image: dw-db
        imagePullPolicy: Never  ## Set to Never as we built the image in the cluster
        ports:
        - containerPort: 12345
          name: disterl
        env:
        - name: PORT
          value: "12345"
        - name: NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        - name: SVC
          valueFrom:
            fieldRef:
              fieldPath: metadata.labels['app']
EOF
```

## Deploy Dockerwatch

Dockerwatch is deployed as previously:

```
> kubectl create service nodeport dockerwatch --tcp=8080:8080 --tcp=8443:8443
service/dockerwatch created
> ./create-certs $(minikube ip)
......
> kubectl create secret generic dockerwatch --from-file=ssl/
secret/dockerwatch created
> eval $(minikube docker-env)
> docker build -t dockerwatch .
```

with some small modifications to the config:

```
cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  ## Name and labels of the Deployment
  labels:
    app: dockerwatch
  name: dockerwatch
spec:
  replicas: 5
  selector:
    matchLabels:
      app: dockerwatch  # has to match .spec.template.metadata.labels
  template:
    metadata:
      labels:
        app: dockerwatch # has to match .spec.selector.matchLabels
    spec:
      containers:
      ## The container to launch
      - name: dockerwatch
        image: dockerwatch
        imagePullPolicy: Never ## Set to Never as we built the image in the cluster
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 8443
          name: https
        volumeMounts:
            - name: kube-keypair
              readOnly: true
              mountPath: /etc/ssl/certs
        env:
        - name: BACKEND
          value: db.default.svc.cluster.local
        - name: IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
      volumes:
        - name: kube-keypair
          secret:
            secretName: dockerwatch
EOF
```

## Testing

We can then test the API using the same curl commands as the [simple demo](https://github.com/erlang/docker-erlang-example/tree/master/advanced_examples/minikube-simple):

```
> curl -H "Content-Type: application/json" -X POST -d "" $(minikube service dockerwatch --url | head -1)/cnt
> curl -H "Content-Type: application/json" -X POST -d "{}" $(minikube service dockerwatch --url | head -1)/cnt
> curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" $(minikube service dockerwatch --url --https | tail -1)
["cnt"]
> curl --cacert ssl/dockerwatch-ca.pem -H "Accept: application/json" $(minikube service dockerwatch --url --https | tail -1)/cnt
{"cnt":2}
```
