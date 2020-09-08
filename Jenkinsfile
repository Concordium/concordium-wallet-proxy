pipeline {
    agent any

    environment {
        DOCKER_BUILDKIT = 1
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                sshagent(credentials: ['6a7625a8-34f4-4c39-b0be-ed5b49aabc16']) {
                    sh 'VERSION_TAG=$(git rev-parse --verify HEAD) && docker build -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/wallet-proxy:$VERSION_TAG -f scripts/Dockerfile --ssh default . --no-cache && docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/wallet-proxy:$VERSION_TAG'
                }
            }
        }
    }
}
