(function ($) {  

  // integer -> string
  var tableHTML = function (numRows) {
    var ans = "<table>";
    for (var i = 0; i < numRows; i++) {
      var rowClass = i % 2 == 0 ? "even" : "odd";
      ans += "<tr class=\"" + rowClass + "\"><td>&nbsp;</td></tr>";
    }
    return ans + "</table>";
  };
  
  // jQuery -> integer
  $.fn.scrollTop = function () {
    return this.get(0).scrollTop;
  };
  
  // jQuery -> integer
  $.fn.scrollBottom = function () {
    return this.get(0).scrollTop + this.height();
  };

  // integer integer -> jQuery
  $.fn.initScrollReport = function (numRows, loadDelay) {
    return this.each(function () {
      
      // element
      //
      // Get references to the viewport and content DIVs:
      var viewport = this;
      var content  = $("div", this).get(0);
      
      // integer
      //
      // Capture the height of a table row by comparing the heights of a two-row and a one-row table.
      var rowHeight = (function () {
        var height1 = $(content).html(tableHTML(2)).height();
        var height2 = $(content).html(tableHTML(1)).height();
        return height1 - height2;
      })();
      
      // integer -> integer
      //
      // Converts a number of pixels into a number of rows.
      var toRows = function (offset) {
        return Math.floor(offset / rowHeight);
      }; 
          
      // (U timer null)
      var loadTimer = null;
      
      // $(content).css("height", (numRows * rowHeight) + "px");
      $(content).html(tableHTML(numRows));
        
      $(viewport).scroll(function (evt) {
        if (loadTimer != null) {
          window.clearTimeout(loadTimer);
          loadTimer = null;
        }
        
        loadTimer = window.setTimeout(function () {
          console.log(
            "Refreshing rows "
            + toRows($(viewport).scrollTop()) 
            + " to "
            + toRows($(viewport).scrollBottom()));
        }, loadDelay);
      });
    });
  };
  
})(jQuery);
