"use strict";

var firebase = require('firebase');
var firebaseui = require('firebaseui');


exports.initializeApp = function() {
  var firebaseConfig = {
    apiKey: "AIzaSyDupytW9bNQ6PALcOYcfwwrbWOR85XSHkM",
    authDomain: "prognosticator-dev.firebaseapp.com",
    databaseURL: "https://prognosticator-dev.firebaseio.com",
    projectId: "prognosticator-dev",
    storageBucket: "prognosticator-dev.appspot.com",
    messagingSenderId: "187805536961",
    appId: "1:187805536961:web:e6d2641ea4878724743a40",
    measurementId: "G-3MDX884TET"
  };
  firebase.initializeApp(firebaseConfig);

  firebase.auth().onAuthStateChanged(function(user) {
    if (user) {
      // User is signed in.
      console.log('user is signed in')
    } else {
      // No user is signed in.
      console.log('user is not signed in')
    }
  });
}


exports.auth = function() {
  var ui = new firebaseui.auth.AuthUI(firebase.auth());
  ui.start('#firebaseui-auth-container', {
    signInOptions: [
      firebase.auth.GoogleAuthProvider.PROVIDER_ID,
    ],
    callbacks: {
      signInSuccessWithAuthResult: function(authResult, redirectUrl) {
        // User successfully signed in.
        // Return type determines whether we continue the redirect automatically
        // or whether we leave that to developer to handle.
        console.log(authResult, redirectUrl);
        return true;
      },
      // uiShown: function() {
      //   // The widget is rendered.
      //   // Hide the loader.
      //   document.getElementById('loader').style.display = 'none';
      // }
    },
    signInFlow: 'popup',
    // signInSuccessUrl: '/',
  });
}
