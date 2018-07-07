"use strict";

var Annoy = require('annoy');

exports.unsafeNew = function(f) {
  return function(metric) {
    return function() {
      return new Annoy(f, metric);
    };
  };
};

exports.unsafeAddItem = function(annoy) {
  return function(i) {
    return function(v) {
      return function() {
        annoy.addItem(i, v);
      };
    };
  };
};

exports.unsafeBuild = function(annoy) {
  return function(n_trees) {
    return function() {
      annoy.build(n_trees);
    };
  };
};

exports.save = function(annoy) {
  return function(path) {
    return function() {
      return annoy.save(path);
    };
  };
};

exports.unsafeLoad = function(annoy) {
  return function(path) {
    return function() {
      return annoy.load(path);
    };
  };
};

exports.unload = function(annoy) {
  return function() {
    annoy.unload();
  };
};

exports.unsafeGetItem = function(annoy) {
  return function(i) {
    return function() {
      return annoy.getItem(i);
    };
  };
};

exports.unsafeGetNNsByItem = function(annoy) {
  return function(i) {
    return function(n) {
      return function(search_k) {
        return function() {
            return annoy.getNNsByItem(i, n, search_k, false);
        };
      };
    };
  };
};

exports.unsafeGetNNsByItem_ = function(annoy) {
  return function(i) {
    return function(n) {
      return function(search_k) {
        return function() {
          return annoy.getNNsByItem(i, n, search_k, true);
        };
      };
    };
  };
};

exports.unsafeGetNNsByVector = function(annoy) {
  return function(v) {
    return function(n) {
      return function(search_k) {
        return function() {
          return annoy.getNNsByVector(v, n, search_k, false);
        };
      };
    };
  };
};

exports.unsafeGetNNsByVector_ = function(annoy) {
  return function(v) {
    return function(n) {
      return function(search_k) {
        return function() {
          return annoy.getNNsByVector(v, n, search_k, true);
        };
      };
    };
  };
};

exports.getNItems = function(annoy) {
  return function() {
    return annoy.getNItems();
  };
};

exports.unsafeGetDistance = function(annoy) {
  return function(i) {
    return function(j) {
      return function() {
        return annoy.getDistance(i, j);
      };
    };
  };
};
