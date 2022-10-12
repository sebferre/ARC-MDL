# build command: docker build -t sferre/arcathon .
FROM fedora:32
WORKDIR /usr/local/bin
COPY arcathon .
CMD ["arcathon"]

# run command: docker run --rm --volume="$(pwd)"/secret_data:/data sferre/arcathon
# run command: docker run --rm --mount type=bind,source="$(pwd)"/secret_data,target=/data sferre/arcathon
