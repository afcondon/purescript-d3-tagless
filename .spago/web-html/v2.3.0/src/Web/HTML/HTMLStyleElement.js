"use strict";

exports.media = function (style) {
  return function () {
    return style.media;
  };
};

exports.setMedia = function (media) {
  return function (style) {
    return function () {
      style.media = media;
    };
  };
};

// ----------------------------------------------------------------------------

exports.type_ = function (style) {
  return function () {
    return style.type;
  };
};

exports.setType = function (type) {
  return function (style) {
    return function () {
      style.type = type;
    };
  };
};
