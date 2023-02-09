from django.shortcuts import render
import environ

env = environ.Env()
environ.Env.read_env()

def home(request):
    return render(request, "eliza.html", context={'eliza_api' : "https://eliza.azurewebsites.net/api/eliza-main?code=WPDE1bAMb37drOBv2N7nEBuXwf8imucXfTOnl81zshxzAzFuuYLDAA=="})


