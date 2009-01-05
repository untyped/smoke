/* Use markup like this:
 *
 *   <div class="show-hide">
 *     <div class="show-hide-switcher">
 *       Click to see a hidden message.
 *     </div>
 *     <div class="show-hide-contents">
 *       Hi mom!
 *     </div>
 *   </div>
 *
 * The switcher and contents are given the extra class "shown"
 * when the content is visible.
 *
 * To save and restore the state of the show/hide across AJAX refreshes, 
 * give the wrapper an ID and change its class to "show-hide-restore".
 * When the element is refreshed, the script will make sure it restores 
 * its previous state.
 *
 * Notes:
 * 
 *   - The script will silently fail if the switcher and content elements
 *     are not immediate children of the wrapper element.
 *
 *   - The save/restore script will silently fail if the wrapper element
 *     doesn't have an ID.
 */

(function($) {

  // hashOf(string, boolean)
  var savedStates = {};

  // boolean -> jQuery
  $.fn.showhide = function (saveState) {
    return this.each(function () {
    
      // string
      var id = this.id;
  
      // Prevent repeat bindings:
      
      if ($(this).data("show-hide-bound")) {
        return;
      } else {
        $(this).data("show-hide-bound", true);
      }
      
      // Bind event listeners:

      // jQuery
      var switcher = $(this).children(".show-hide-switcher");
      var contents = $(this).children(".show-hide-contents");
      
      if (switcher.length == 0 || contents.length == 0) {
        return;
      }
      
      switcher.click(function (evt) {
        contents.slideToggle("fast", function () {
          switcher.toggleClass("shown");
          contents.toggleClass("shown");
          if (saveState && id) {
            savedStates[id] = switcher.hasClass("shown");
          }
        });
      });
      
      // Restore saved state:
      
      if (saveState && id) {
      
        // We have to have true *and* false tests here:
        // the third option is undefined (i.e. no saved state).
        
        if (savedStates[id] == true) {
          
          contents.show();
          switcher.addClass("shown");
          contents.addClass("shown");
          
        } else if (savedStates[id] == false) {
          
          switcher.removeClass("shown");
          contents.removeClass("shown");
          contents.hide();
          
        }
      }      
    });
  };
  
  $(document).ready(function (evt) {
    $(document).bind("smoke-page-update", function (evt) {
      $(".show-hide").showhide(false);
      $(".show-hide-restore").showhide(true);
    });
  });
})(jQuery);
