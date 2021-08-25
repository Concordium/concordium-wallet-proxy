pipeline {
    agent any

    environment {
        image_repo = 'concordium/wallet-proxy'
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('dockerhub-login') {
            environment {
                // Defines 'CRED_USR' and 'CRED_PSW'
                // (see 'https://www.jenkins.io/doc/book/pipeline/jenkinsfile/#handling-credentials').
                CRED = credentials('jenkins-dockerhub')
            }
            steps {
                sh 'echo "${CRED_PSW}" | docker login --username "${CRED_USR}" --password-stdin'
            }
        }
        stage('build') {
            steps {
                sh '''\
                    docker build \
                      -t "${image_name}" \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label git_commit="${GIT_COMMIT}" \
                      .
                '''
            }
        }

        stage('push') {
            steps {
                sh 'docker push "${image_name}"'
            }
        }
    }
}
