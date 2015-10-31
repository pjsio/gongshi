devtools::install_github("geoffjentry/twitteR")
library(twitteR)
api_key             <- "9wpEt5xzZccmFjbXr9eXRnn0V"
api_secret          <- "Fllk4rPet3ecIkB0DiFatmLWIYA7jmVk5scDgVfoEjnoeRYWlm"
access_token        <- "4073747353-WtW4VCCXVBw4UTxfWpA4HxNYqBFbX7NuaP9ukh6"
access_token_secret <- "FVQ4wp4maYluYmR1BOiZvWOgzBSn57fCDi5tVvapVvMaq"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


## tweet() function 안에 넣고 싶은 string을 대입, image 는 mediaPath 를 설정
tweet('hello world2')
tweet('imagetest', mediaPath = 'turing.jpg')