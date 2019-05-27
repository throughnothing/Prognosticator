// module Express.Foreign
"use strict";

exports.jsonBodyParser = require('body-parser').json();

exports._getField = function(nothing, just, req, field) {
  return function() {
    if(req[field] && req[field] != null) {
      return just(req[field]);
    } else {
      return nothing;
    }
  };
};

exports.sessionMiddleware = function(secret) {
  const cookieSession = require('cookie-session');
  return cookieSession(
    { secret: secret
    , name: 'session'
    , maxAge: (24 * 60 * 60 * 1000) * 30 // 30 days
    });
}

exports.mkPassport = function(conf) {
  return function(findOrCreateUser) {
    const passport = require('passport');
    const GoogleStrategy = require('passport-google-oauth20').Strategy;

    passport.use(
      new GoogleStrategy(
        { clientID: conf.google.clientId
        , clientSecret: conf.google.clientSecret
        , callbackURL: conf.google.callbackURL
        },
        function (at, rt, profile, done) {
          // profile._json looks like this:
          //{
          //   sub: '1107',
          //   name: 'Will Wolf',
          //   given_name: 'Will',
          //   family_name: 'Wolf',
          //   picture: 'https://lh5.googleusercontent.com/-33u_OO_hc9c/AAAAAAAAAAI/AAAAAAAAAAc/dQQx5_od7tg/photo.jpg',
          //   email: 'will@polychain.capital',
          //   email_verified: true,
          //   locale: 'en',
          //   hd: 'polychain.capital'
          // }

          const p = profile._json;
          findOrCreateUser(
            { google_id: p.sub
            , name: p.name
            , email: p.email
            , picture: p.picture
            })()
            .then(function(user){ done(null, user)})
            .catch(function(err){ console.log(err); done(err, null); });
        }
      )
    );

    passport.serializeUser(function(user, done) { done(null, user) });
    passport.deserializeUser(function(user, done) { done(null, user) });
    return passport;
  };
}

exports.passportInitializeMiddleware = function(passport) {
  return passport.initialize();
};

exports.passportSessionMiddleware = function(passport) {
  return passport.session();
};

exports._passportAuthenticateHandler = function(passport) {
  return function(req, res, next) {
    return function() {
      passport.authenticate('google',
        { scope: ['profile', 'email']
        , accessType: 'offline'
        })(req,res,next);
    };
  };
};

exports._passportAuthenticateCallbackHandler = function(options) {
  return function(passport) {
      return function(req, res, next) {
        return function() {
          passport.authenticate('google', options)(req, res, next);
        }
      }
  };
};




