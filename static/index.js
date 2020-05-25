var Main = require('../output/UI.Main');

function main () {
  document.addEventListener("DOMContentLoaded", function(){
    // Clear the body dom before roloading app
    window.document.getElementById('app').innerHtml = "";
    Main.main();
  });
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    main();
  });
}

console.log('Starting app');

main();