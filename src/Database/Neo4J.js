"use strict";

// module Database.Neo4J
/* eslint-env node*/
/* eslint "no-underscore-dangle": 0 */

var g = typeof global !== 'undefined' ? global : window;
g.WebSocket = (typeof g.WebSocket !== 'undefined') ? g.WebSocket : require('../../node_modules/ws/index');
g.neo4j = (typeof g.neo4j !== 'undefined') ? g.neo4j : require('../../bower_components/neo4j-driver/lib/browser/neo4j-web');

function getNeo4J() {
  return g.neo4j.v1;
}

exports.mkAuth_ = function mkAuth_(username) {
  return function(password) {
    return getNeo4J().auth.basic(username, password);
  };
};

exports.mkDriver_ = function mkDriver_(url, auth, opts) {
  var neo4j = getNeo4J();
  return neo4j.driver(url, auth, opts);
};

exports.mkSession_ = function mkSession_(driver) {
  return driver.session();
};

exports.runQuery_ = function runQuery_(error, success, session, query, params) {
  session
    .run(query, params)
    .then(function (result) {
      var records = [];
      for (i = 0; i < result.records.length; i++) {
        records.push(result.records[i]);
      }
      success(result.records);
    })
    .catch(error);
};

exports.closeSession_ = function closeSession_(callback, session) {
  session.close(callback);
};

exports.closeDriver_ = function closeDriver_(callback, driver) {
  driver.close(callback);
};
