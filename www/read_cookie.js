// read the user login token from a cookie (new protocol) 
Shiny.addCustomMessageHandler("read_cookie", function(message) {
    readCookie();
});

function readCookie() {
  const xhr = new XMLHttpRequest();
  const url = 'https://www.synapse.org/Portal/sessioncookie';
  xhr.withCredentials = true;
  xhr.onreadystatechange = function() {
    if (xhr.readyState == XMLHttpRequest.DONE) {
      Shiny.onInputChange("cookie", xhr.responseText);
    }
  }
  xhr.open("GET", url);
  xhr.send();
}
