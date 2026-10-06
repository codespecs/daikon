# Dockerfiles for Daikon

This directory contains Dockerfiles to create new Docker images for
running tests reproducibly.

The Dockerfiles are generated from `Dockerfile-*.m4`.
To regenerate them, run `make dockerfiles`.
The JDK versions are listed in variable `DOCKERFILE_JDKS` in `Makefile`.
When you change them, also change the list of `create_upload_docker_image`
commands below.
If a JDK version is not yet available as an OS package, edit `jdk_packaged`
and `jdk_download_url` in `Dockerfile-defs.m4`.

The rest of this file explains how to build new Docker images.

## Preliminaries

```sh
# Finish docker setup if necessary.
sudo usermod -aG docker $(whoami)
# Then log out and back in.

# Obtain Docker credentials.
# (This is only necessary once per machine; credentials are cached.)
docker login
```

## Cleanup

After running any of the below, consider deleting the docker containers,
which can take up a lot of disk space.

To stop and remove/delete *all* docker containers:

```sh
docker stop $(docker ps -a -q)
docker rm -vf $(docker ps -aq)
```

To remove all images:

```sh
docker rmi -f $(docker images -aq)
```

To remove most everything, including build cache objects:

```sh
docker system prune -a -f
```

## Create the Docker images

To create all the Docker images and upload them to Docker Hub, run:

```sh
make docker-images && git push
```

To create and upload one image, run, for example:

```sh
make docker-image-ubuntu-jdk21-plus
```

To create images named `mdernst/daikon-*-testing`, pass
`DOCKERTESTING=-testing` to `make`.  To make CI use those images, update the
value of `docker_testing` in file `.azure/defs-common.m4`, then regenerate
the CI configuration files by running, from the top-level directory:

```sh
make -C .azure && make -C .circleci && make -C .github/workflows
```
