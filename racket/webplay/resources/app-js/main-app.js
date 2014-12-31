// Standard global variables and functions for simple fade-in fade-out animations

var AnimDuration = 1000;

var transContent = function  (domElmt) {
    // body...
    var content = document.getElementById(domElmt);
    Jacked.fadeIn(content, {duration: AnimDuration});
};

Array.prototype.remove = function(from, to) {
  var rest = this.slice(parseInt(to || from) + 1 || this.length);
  this.length = from < 0 ? this.length + from : from;
  return this.push.apply(this, rest);
};