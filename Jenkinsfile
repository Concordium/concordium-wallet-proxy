pipeline {
    agent any

    environment {
        image_repo = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/wallet-proxy'
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                sshagent(credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        docker build \
                          -t "${image_name}" \
                          --build-arg base_image_tag="$base_image_tag" \
                          --label base_image_tag="$base_image_tag" \
                          --label git_commit="${GIT_COMMIT}" \
                          -f scripts/Dockerfile \
                          --ssh default \
                          --no-cache \
                          .
                    '''
                }
            }
        }

        stage('push') {
            steps {
                sh 'docker push "${image_name}"'
            }
        }
    }
}
