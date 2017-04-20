{
    for(i=1; i<=NF; i++) {
        time = $i;
        sub(/[a-z]+$/, "", time);
        unit = $i;
        sub(/^[^a-z]+/, "", unit);

        # convert to nanoseconds
        if (unit == "us") {
            time *= 1;
        } else if (unit == "ms") {
            time *= 1000;
        } else if (unit == "s") {
            time *= 1000000;
        }
        printf int(time) " "
    }
}
