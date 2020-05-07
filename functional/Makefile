DOCKER_USER=dublinlg

PROJECT_USER=dublinlg
PROJECT_NAME=functionalcode

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}

CONTAINER_NAME=functional_code





docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} -f Dockerfile .

docker-run:
	docker run --rm -d \
	  -p 8787:8787 \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_NAME}":rw \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
	  --name ${CONTAINER_NAME} \
	  ${IMAGE_TAG}

docker-stop-all:
	docker stop $(shell docker ps -q -a)

docker-stop:
	docker stop ${CONTAINER_NAME}

docker-clean:
	docker rm $(shell docker ps -q -a)

