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

exports.connect_ = function connect_() {
  return function(success, error) {
    var neo4j = getNeo4J();
  };
};
