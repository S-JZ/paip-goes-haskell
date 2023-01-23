from django.shortcuts import render
import environ

env = environ.Env()
environ.Env.read_env()

def home(request):
    return render(request, "eliza.html", context={'eliza_api' : env('ELIZA')})


