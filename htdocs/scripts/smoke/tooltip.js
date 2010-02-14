/* Adapted from a tutorial by Remy Sharp (http://jqueryfordesigners.com/coda-popup-bubbles/).
 *
 * Use markup like this:
 *
 *   <span class="tooltip-anchor">
 *     Hover over me for a tooltip.
 *     <div class="tooltip">
 *       This is the tooltip.
 *     </div>
 *   </span>
 *
 * and code like this:
 *
 *   jQuery.tooltip(".tooltip-anchor");
 *
 * the options are:
 *
 *   - offsetX:       the X offset of the tooltip relative to the anchor or viewport;
 *   - offsetY:       the Y offset of the tooltip relative to the anchor or viewport;
 *   - scrollbarSize: an extra allowance for the size of scrollbars;
 *   - time:          the duration of the show/hide animation;
 *   - delay:         the delay before the tooltip hides when it loses focus.
 *
 * The last couple of lines of code hook into Smoke's smoke-page-update event.
 *
 * Once the tooltip has been set up, the element is annotated with:
 *
 *   jQuery(blah).data("smoke-tooltip-bound", true);
 *
 * to make sure no repeat bindings occur.
 */

(function($) {

  // -> { top: natural, left: natural }
  $.fn.absolutePosition = function () {
    if (this.get(0).offsetParent) {
      var pos = this.position();
      var parentPos = this.offsetParent().absolutePosition();
      return {
        top: pos.top + parentPos.top,
        left: pos.left + parentPos.left
      };      
    } else {
      return { top: 0, left: 0 };
    }
  };

  $.fn.moveToBody = function () {
    return this.each(function () {
      var src = $(this).data("original-location");
      
      if (!src) {
        var src = $("<span></span>").insertBefore(this);
        $(this).data("breakout-source", src);
        $(this).remove().appendTo($("body"));
      }
    });
  };

  $.fn.returnFromBody = function () {
    return this.each(function () {
      var src = $(this).data("original-location");
      
      if (src) {
        return;
        $(this).remove().insertAfter(src);
        src.remove();
      }
    });
  };
  
  // string object -> jQuery
  $.fn.tooltip = function (tooltipClass, settings) {
    tooltipClass = tooltipClass || "tooltip";
  
    settings = jQuery.extend({
      offsetX:       7,
      offsetY:       7,
      scrollbarSize: 20,
      time:          150,
      delay:         250,
      mode:          "hover"
    }, settings);
      
    // integer
    var offsetX       = settings.offsetX;
    var offsetY       = settings.offsetY;
    var scrollbarSize = settings.scrollbarSize;
    var time          = settings.time;
    var delay         = settings.delay;
        
    return this.each(function () {
    
      // Prevent repeat bindings:
      if ($(this).data("tooltip-bound")) {
        return;
      } else {
        $(this).data("tooltip-bound", true);
      }

      $(this).removeAttr("title");

      // timerID
      var delayTimer = null;
  
      // boolean
      var appearing = false;
      var visible = false;

      // element      
      var anchor = $(this);
      var tooltip = $("." + tooltipClass, this).css("opacity", 0).moveToBody();

      $([this, tooltip.get(0)]).mouseover(function () {
        Smoke.log("over");

        if (delayTimer) {
          clearTimeout(delayTimer);
        }
  
        if (!appearing && !visible) {
          appearing = true;

          // reset position of tooltip box
          tooltip.css({
            display: "block",
            position: "absolute",
            zIndex: 1000001
          }).position({
            my: "left top",
            at: "left bottom",
            of: anchor,
            collision: "flip"
          }).animate(
            { opacity: 1 },
            time,
            "swing",
            function () {
              // once the animation is complete, set the tracker variables
              appearing = false;
              visible = true;
            });
        }
        
        return this;
      }).mouseout(function () {
        Smoke.log("out");
      
        if (delayTimer) {
          clearTimeout(delayTimer);
        }
        
        delayTimer = setTimeout(function () {
          delayTimer = null;
          tooltip.animate({ opacity: 0 }, time, "swing", function () {
            visible = false;
            tooltip.css("display", "none");
          }).returnFromBody();
        }, delay);
        
        return this;
      });
    });
  };
  
  $(document).ready(function (evt) {
    $(".tooltip-anchor").live("mouseover", function () {
      if(!$(this).data("tooltip-bound")) {
        $(this).tooltip("tooltip");
        $(this).mouseover();
      }
    });
  });
})(jQuery);
