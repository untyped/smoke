/* Scripts for the autocomplete-field% component.
 */
 
(function ($) {

  // element
  var popup = $(document.createElement("div"))
    .attr("id", "smoke-autocomplete-popup").get(0);
  
  // element
  var autocompleteFocused = null;
  
  // boolean
  var popupVisible = false;
  var popupHovered = false;

  // integer
  var position = -1;
  
  // Recreate the DIV whenever   
  $(document).bind("smoke-page-update", function () {

    // Reattach the popup DIV if it has been detached:
    if ($("#smoke-autocomplete-popup").length == 0) {
      autocompleteFocused = null;
      popupVisible = false;
      popupHovered = false;
      position = -1;

      $(popup).unbind().hide().addClass("smoke-autocomplete-options");
          
      $("body").append(popup); 
    }

  });
  
  // element -> array(integer integer)
  var findPos = function (elem) {
    var left = elem.offsetLeft || 0;
    var top = elem.offsetTop || 0;
    while (elem = elem.offsetParent) {
      left += elem.offsetLeft || 0;
      top += elem.offsetTop || 0;
    }
    return [left, top];
  }
  
  // element arrayOf(string) -> void
  var showPopup = function (elem, data) {
  
    // array(integer integer)
    var elemPos = findPos(elem);
    
    // string
    var html = "<table>";
    $(data).each(function (index, option) {
      html += "<tr>";
      $(option).each(function (index, value) {
        html += "<td>" + value + "</td>";
      });
      html += "</tr>";
    });
    html += "</table>";
    
    // Initialise popup:
    
    $(popup)
      .html(html)
      .css({ width: $(elem).width() + "px",
             left:  elemPos[0] + "px",
             top:   elemPos[1] + elem.offsetHeight + "px" })
      .fadeIn("fast");
    
    $("table", popup).hover(
      // mouseover
      function (evt) {
        popupHovered = true;
      },
      // mouseout
      function (evt) {
        popupHovered = false;
        if (!autocompleteFocused) {
          hidePopup(null);
        }
      });

    $("tr", popup).each(function (index) {
      $(this).click(function (evt) {
        position = index;
        selectItem(elem);
        hidePopup();
      });
    });
    
    if (position >= 0) {
      $("tr:eq(" + position + ")", popup).addClass("current");
    }
  };
        
  // -> void
  var hidePopup = function () {
    $("*", popup).unbind();
    $(popup).fadeOut("fast");
    position = -1;
  };
  
  // integer -> jQuery
  var itemAt = function (pos) {
    return $("tr:eq(" + pos + ")", popup);
  };
  
  // element arrayOf(string) -> void
  //
  // Moves the cursor up/down the list a specified amount.
  var moveCursor = function (elem, delta) {
    // integer
    var first = -1;
    var last = $("tr", popup).length;
    
    if (position > first) {
      itemAt(position).removeClass("current");
    }
    
    position += delta;

    if (position < first) {
      position = last - 1;
    } else if (position >= last) {
      position = first;
    }
    
    itemAt(position).addClass("current");
  };
      
  // element -> void
  var selectItem = function (elem) {
    if (position >= 0) {
      elem.value = itemAt(position).children("td:first").text();
      $(elem).trigger("change");
    }
  };
  
  // integer -> void
  //
  // Utility function: 
  // avoids redundancy in the event handlers below.
  //
  // If delta is 0, shows the popup; 
  // if delta is non-0, moves the cursor on the popup.
  var moveOrShow = function (elem, delta, url, minTriggerLength) {
    window.setTimeout(function () {
      if (elem.value.length >= minTriggerLength) {
        $.get(url,
          { prefix: elem.value },
          function (script, status) {
            showPopup(elem, eval(script));
            if (delta == 0) {
              // showPopup(elem, eval(script));
            } else {
              moveCursor(elem, delta);
            }
          });
      } else {
        hidePopup();
      }
    });
  };
  
  Smoke.Autocomplete = {};
  
  // element event string integer -> void
  //
  // minTriggerLength is the minimum number of characters
  //   in the field required to trigger a popup
  Smoke.Autocomplete.onKeyDown = function (elem, evt, url, minTriggerLength) {
    switch (evt.keyCode) {
      case 13: // return
        evt.preventDefault();
      case 9:  // tab
        window.setTimeout(function () {
          selectItem(elem);
          hidePopup();
        });
        break;
      case 27: // escape
        window.setTimeout(function () {
          hidePopup();
        });
        break;
      case 38: // up
        evt.preventDefault();
        moveOrShow(elem, -1, url, minTriggerLength);
        break;
      case 40: // down
        evt.preventDefault();
        moveOrShow(elem, 1, url, minTriggerLength);
        break;
      default: // everything else
        moveOrShow(elem, 0, url, minTriggerLength);
    };
  };
  
  // element event string number -> void
  //
  // minTriggerLength is the minimum number of characters
  //   in the field required to trigger a popup
  Smoke.Autocomplete.onClick = function (elem, evt, url, minTriggerLength) {
    if (popupVisible) {
      hidePopup();
    } else {
      moveOrShow(elem, 0, url, minTriggerLength);
    }
  };
  
  // element event -> void
  Smoke.Autocomplete.onFocus = function (elem, evt) {
    autocompleteFocused = elem;
  };
  
  // element event -> void
  Smoke.Autocomplete.onBlur = function (elem, evt) {
    autocompleteFocused = null;
    if (!popupHovered) {
      selectItem(elem);
      hidePopup();
    }
  };
})(jQuery);
