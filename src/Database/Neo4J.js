"use strict";

// module Database.Neo4J
/* eslint-env node*/
/* eslint "no-underscore-dangle": 0 */

var g = typeof global !== 'undefined' ? global : window;
g.WebSocket = (typeof g.WebSocket !== 'undefined') ? g.WebSocket : require('ws');
g.neo4j = (typeof g.neo4j !== 'undefined') ? g.neo4j : require('neo4j-driver/lib/browser/neo4j-web.min');

function getNeo4J() {
  return g.neo4j.v1;
}

function convertToObject(record) {
  var rec = {};
  record.forEach(function(value, key) {
    rec[key] = value;
  });
  return rec;
}

exports.toNeoInt = function(i) {
  return getNeo4J().int(i);
};

exports.stringToNeoInt = function(x) {
  return getNeo4J().int(x);
};

exports.fromNeoInt = function (n) {
  return n.toInt();
};

exports.mkDriver_ = function mkDriver_(url, auth, opts) {
  var neo4j = getNeo4J();
  return function() {
    return neo4j.driver(url, auth, opts);
  };
};

exports.mkSession_ = function mkSession_(driver) {
  return function() {
    return driver.session();
  };
};

exports.runQuery_ = function runQuery_(mkError, reject, accept, transaction, query, params) {
  return function() {
    transaction
      .run(query, params)
      .then(function (result) {
        var records = [];
        for (var i = 0; i < result.records.length; i++) {
          records.push(convertToObject(result.records[i]));
        }
        console.log(records);
        accept(records)();
      })
      .catch(function (err) {
        var myError = mkError(err.message);
        reject(myError)();
      });
  };
};

exports.beginTransaction_ = function beginTransaction_(session) {
  return function() {
    return session.beginTransaction();
  };
};

exports.commitTransaction_ = function commitTransaction_(transaction) {
  return function() {
    return transaction.commit();
  };
};

exports.rollbackTransaction_ = function rollbackTransaction_(transaction) {
  return function() {
    return transaction.rollback();
  };
};

exports.closeSession_ = function closeSession_(session) {
  return function() {
    session.close();
  };
};

exports.closeDriver_ = function closeDriver_(driver) {
  return function() {
    driver.close();
  };
};
