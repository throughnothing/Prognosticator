"use strict"

// https://stackoverflow.com/questions/3340055/how-to-check-if-new-datesome-date-was-able-to-parse-correctly
exports._parse = function(nothing) {
  return function(just) {
    return function(str) {
      const d = new Date(str);
      if(isNaN(d)) {
        return nothing;
      } else {
        return just(d);
      }

    }
  }
}

exports.toISOString = function(dt) {
  return dt.toISOString();
}