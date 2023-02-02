
const statesElem = document.querySelector("#states-input-area")
const goalsElem = document.querySelector("#goals-input-area")
const answerElem = document.querySelector("#answer")
const submitBtn = document.querySelector("#submit")

let statesText = ""
let goalsText = ""

let gpsBaseURL = "https://eliza.azurewebsites.net/api/gps?code=F0wwiGHDQh6Phxu7-mbM5Cr70YhuX_uBm4MeUExvqkNQAzFuPEaStQ==&"

const prefix = "Executing"
const requestOptions = {
  method: 'GET',
  redirect: 'follow'
}

statesElem.addEventListener('keyup', (event) => {
  statesText = event.target.value
})

goalsElem.addEventListener('keyup', (event) => {
  goalsText = event.target.value
})

const formatResult = (result) => {
  return result.split(prefix).slice(1).map(action => prefix + action)
}

const displayAction = (action) => {
  if (action.length > 0)
    answerElem.innerText += (action + "\n")
}

const unsolvedDisplay = (result) => {
  if (result.length < 0) {
    answerElem.innerText = "Does not reach goal state"
  }
}

const displayResult = (result) => {
  unsolvedDisplay(result)
  const results = formatResult(result)
  results.forEach(action => {
    displayAction(action)
  })
}

submitBtn.addEventListener('click', (event) => {
  const gpsAPIURL = gpsBaseURL + "&states=" + statesText.trim() + "&goal=" + goalsText.trim()
  answerElem.innerHTML = `<div class="loader"></div>`
  fetch(gpsAPIURL, requestOptions)
  .then(response => response.text())
  .then(result => { 
      displayResult(result)
  })
  .catch(error => console.log('error', error));
})