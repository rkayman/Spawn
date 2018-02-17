docker run -d --net=host --name=kafka -e KAFKA_ZOOKEEPER_CONNECT=localhost:32181 -e KAFKA_ADVERTISED_LISTENERS=PLAINTEXT://localhost:29092 confluentinc/cp-kafka:3.0.0

