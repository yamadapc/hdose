'use strict';
var assert = require('assert');

describe('something', function() {
  it('passes after 1 sec', function(done) {
    setTimeout(function() {
      done()
    }, 1000);
  });

  it('passes but gets interupted on changes', function(done) {
    setTimeout(function() {
      done();
    }, 1000);
  });
});
