pipeline {
    agent any
    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                sh 'VERSION_TAG=$(git rev-parse --verify HEAD) && docker build -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/wallet-proxy:$VERSION_TAG -f scripts/Dockerfile --ssh default . --no-cache && docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/wallet-proxy:$VERSION_TAG'
            }
        }
    }
}