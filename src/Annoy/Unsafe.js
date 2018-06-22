"use strict";

var Annoy = require('annoy');

exports.unsafeNewAnnoy = function(f) {
  return function(metric) {
    return function() {
      return new Annoy(f, metric);
    };
  };
};

exports.unsafeAddItem = function(i) {
  return function(v) {
    return function(annoy) {
      return function() {
        annoy.addItem(i, v);
      };
    };
  };
};

exports.unsafeBuild = function(n_trees) {
  return function(annoy) {
    return function() {
      annoy.build(n_trees);
    };
  };
};

// exports.save = function(annoy) {
//   return function(path) {
//     return function() {
//       annoy.save(path);
//     };
//   };
// };

// exports.load = function(annoy) {
//   return function(path) {
//     return function() {
//       annoy.load(path);
//     };
//   };
// };

// exports.unload = function(annoy) {
//   return function() {
//     annoy.unload();
//   };
// };

exports.unsafeGetItem = function(i) {
  return function(annoy) {
    return annoy.getItem(i);
  };
};

// exports.getNNsByVector

// exports.getNNsByItem

// exports.getNItems

// exports.getDistance
