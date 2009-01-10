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
  
  // string object -> jQuery
  $.fn.tooltip = function (tooltipClass, settings) {
    tooltipClass = tooltipClass || "tooltip";
  
    settings = jQuery.extend({
      offsetX:       7,
      offsetY:       7,
      scrollbarSize: 20,
      time:          150,
      delay:         250
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

      $(this).attr("title", null);

      // timerID
      var animationTimer = null;
  
      // boolean
      var appearing = false;
      var visible = false;

      // element      
      var anchor = $(this);
      var tooltip = $("." + tooltipClass, this).css("opacity", 0);

      $([this, tooltip.get(0)]).mouseover(function () {

        if (animationTimer) {
          clearTimeout(animationTimer);
        }
  
        if (!appearing && !visible) {
          appearing = true;

          // absolute coordinates of the viewport edges:
          var viewLeft   = $(window).scrollLeft();
          var viewRight  = $(window).width() + viewLeft;
          var viewTop    = $(window).scrollTop();
          var viewBottom = $(window).height() + viewTop;
          
          // size of the tooltip:
          var toolWidth  = $(tooltip).width();
          var toolHeight = $(tooltip).height();
          
          // absolute position of the anchor:
          var anchorPos  = $(anchor).absolutePosition();
          var anchorLeft = anchorPos.left;
          var anchorTop  = anchorPos.top;
          
          // adjustment to keep the tooptip on-screen:
          var posX = Math.max(viewLeft + offsetX,
            Math.min(anchorLeft + offsetX,
              viewRight - toolWidth - offsetX - scrollbarSize)) - anchorLeft;
          var posY = Math.max(viewTop + offsetY, 
            Math.min(anchorTop + offsetY,
              viewBottom - toolHeight - offsetY - scrollbarSize)) - anchorTop;
          
          console.log(anchor.absolutePosition());
          
          // reset position of tooltip box
          tooltip.css({ left: posX, top: posY });
  
          tooltip.css({ display: "block" }).animate({ opacity: 1 }, time, "swing", function () {
            // once the animation is complete, set the tracker variables
            appearing = false;
            visible = true;
          });
        }
        
        return this;
      }).mouseout(function () {
      
        if (animationTimer) {
          clearTimeout(animationTimer);
        }
        
        animationTimer = setTimeout(function () {
          animationTimer = null;
          tooltip.animate({ opacity: 0 }, time, "swing", function () {
            visible = false;
            tooltip.css("display", "none");
          });
        }, delay);
        
        return this;
      });
    });
  };
  
  $(document).ready(function (evt) {
    $(document).bind("smoke-page-update", function (evt) {
      $(".tooltip-anchor").tooltip("tooltip");
    });
  });
})(jQuery);
