# build command: docker build -t sferre/arcprize .
FROM fedora:39
WORKDIR /usr/local/bin
COPY arcprize .
CMD ["arcprize"]

# run command: docker run --rm -v /kaggle/input/arc-prize-2024:/data -v /kaggle/working:/etc sferre/arcprize

## run command: docker run --rm --volume="$(pwd)"/secret_data:/data sferre/arcathon
## run command: docker run --rm --mount type=bind,source="$(pwd)"/secret_data,target=/data sferre/arcathon
