/* Match this to markup like this:
 *
 *   <img class="rollover" src="blah.png" />
 *
 * and create the tooltip like this:
 *
 *   jQuery(".rollover").rollover("-hover");
 *
 * The rollover filename is calculated from the filename of the normal image 
 * and the argument passed to rollover():
 *
 *   "blah.png" -> "blah-hover.png"
 */
 
(function ($) {

  // string string -> string
  var addSuffix = function (path, suffix) {
    var extensionPosition = path.lastIndexOf(".");
    return extensionPosition == -1
      ? path + suffix
      : path.substring(0, extensionPosition) + suffix + path.substring(extensionPosition);
  };
  
  // string string -> string
  var removeSuffix = function (path, suffix) {
    return path.replace(new RegExp(suffix), "");
  };

  // string -> jquery
  $.fn.rollover = function (suffix) {
    if ($(this).data("smoke-rollover-bound")) {
      return;
    } else {
      $(this).data("smoke-rollover-bound", true);
    }
    
    return this.hover(
      function () { $(this).attr("src", addSuffix($(this).attr("src"), suffix)); },
      function () { $(this).attr("src", removeSuffix($(this).attr("src"), suffix)); });
  };
  
  $(document).ready(function (evt) {
    $("img.rollover").rollover("-hover");
    $(document).bind("smoke-page-update", function (evt) {
      $("img.rollover").rollover("-hover");
    });
  });
})(jQuery);
