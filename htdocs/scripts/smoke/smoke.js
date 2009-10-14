var Smoke = {};
var DeliriumClient = {};

if (!window.console) {
  window.console = {};
}

if (!window.console.log) {
  window.console.log = function () {
    // Do nothing
  };
}

(function ($) {

  // any ... -> void
  Smoke.log = (function () {
    // Firebug:
    if(window.console && window.console.firebug){
      return function () {
        window.console.log.apply(this, arguments);
      };
    // Browsers with a console (Safari):
    } else if (window.console && window.console.log) {
      return function () {
        var str = "";
        for (var i = 0; i < arguments.length; i++) {
          str += i > 0 ? " " + arguments[i] : "" + arguments[i];
        }
        window.console.log(str);
      };
    // Other browsers:
    } else {
      return function () {
        // Do nothing
      };
    };
  })();
  
  // string -> (U elem null)
  Smoke.findById = function (id) {
    return document.getElementById(id);
  };
  
  // (U string null)
  Smoke.currentPage = null;
  
  // (U HTMLElement null)
  Smoke.documentHead = null;
  Smoke.documentBody = null;
  
  // string string (-> void) -> void
  Smoke.initialize = function (currentPage, formID, initComponents) {
    Smoke.currentPage = currentPage;
    Smoke.documentHead = $("head").get(0);
    Smoke.documentBody = $("body").get(0);
    initComponents();
    Smoke.triggerUpdateEvent(true);
  };
  
  // Submit and update events ====================
  
  // boolean -> boolean
  Smoke.triggerSubmitEvent = function (fullRefresh) {
    return $(document).trigger("smoke-page-submit", arguments);
  };
  
  // boolean -> boolean
  Smoke.triggerUpdateEvent = function (fullRefresh) {
    return $(document).trigger("smoke-page-update", arguments);
  };
  
  // Tracking focus across AJAX refreshes ========
  
  // (U string null)
  Smoke.focusedId = null;
  
  var focusListener = function (evt) {
    if(evt.target && evt.target.id) {
      Smoke.focusedId = evt.target.id;
    }
  };
  
  var blurListener = function (evt) {
    Smoke.focusedId = null;
  };
  
  $(document).bind("smoke-page-update", function () {
    $("select,textarea,input").unbind("focus", focusListener);
    $("select,textarea,input").unbind("blur",  blurListener);
    $("select,textarea,input").bind("focus", focusListener);
    $("select,textarea,input").bind("blur",  blurListener);
    if(Smoke.focusedId) {
      $("#" + Smoke.focusedId).focus();
    }
  });
  
  // Logging messages ============================
  
  // exception [string] -> void  
  Smoke.badAttach = function (exn, id) {
    Smoke.log("Failed to attach", exn, id || "no id provided");
  };
  
  // exception [string] -> void  
  Smoke.badRender = function (exn, id) {
    Smoke.log("Failed to render", exn, id || "no id provided");
  };

  // exception [string] -> void  
  Smoke.badDetach = function (exn, id) {
    Smoke.log("Failed to detach", exn, id || "no id provided");
  };
  
  // Submit data =================================
  
  // hashOf(string string)
  // Hash of form name (or pseudo-name) to value.
  Smoke.submitData = {};
  
  // string -> any
  Smoke.getSubmitData = function (key) {
    return Smoke.submitData[key];
  };
  
  // string any -> void
  Smoke.setSubmitData = function (key, val) {
    Smoke.submitData[key] = val;
  };
  
  // string -> void
  Smoke.removeSubmitData = function (key) {
    delete Smoke.submitData[key];
  };
  
  // AJAX ========================================
    
  // (U string (arrayOf string)) [object] -> void
  //
  // url can be a string or an array, with the base callback url at index 0
  // and the arguments at indices 1 and up
  Smoke.doAjax = function (url, data) {
    var request = null;
    
    try {
      if (!Smoke.triggerSubmitEvent(false)) {
        return false;
      };
      
      // Convert array-form URL into a string URL:
      var url = (typeof url == "string")
        ? url
        : url[0] + "/" + $.map(url.slice(1), encodeURIComponent).join("/");
  
      var data = data
        ? $.extend(Smoke.submitData, data)
        : Smoke.submitData;
      
      Smoke.submitData = {};
      
      // The result JS is automatically evaluated by jQuery:
      request = $.ajax({
        async      : true,
        global     : false,
        url        : url,
        type       : "post",
        data       : data,
        beforeSend : function (xhr) {
                       xhr.setRequestHeader("Content-Type",
                         "application/x-www-form-urlencoded");
                       if (Smoke.currentPage) {
                         xhr.setRequestHeader("X-Smoke-Page",
                           Smoke.currentPage);
                       }
                     },
        dataType   : "text",
        success    : function (responseText) {
                       eval(responseText);
                       // Smoke.triggerUpdateEvent(false);
                     },
        error      : Smoke.onAjaxFailure });
    } catch (exn) {
      Smoke.onAjaxFailure (request, "Could not send background request", exn);
    }
  };
  
  // (objectOf string integer)
  //
  // A map of url => timeoutId for pending AJAX requests.
  Smoke.delayedAjaxIds = {};
  
  // (objectOf string object)
  //
  // A map of url => postdata for pending AJAX requests.
  Smoke.delayedAjaxData = {};
      
  // natural (U string (arrayOf string)) [object] -> void
  //
  // Send data to url after time milliseconds. If another call to delayAjax occurs
  // within time, the data objects are appended and sent in a single request.
  Smoke.doDelayedAjax = function (time, url, data) {
    data = data || {};
  
    var oldId = Smoke.delayedAjaxIds[url];
    if(oldId) {
      window.clearTimeout(oldId);
      data = $.extend(Smoke.delayedAjaxData[url], data);
    }

    Smoke.delayedAjaxData[url] = data;
    Smoke.delayedAjaxIds[url] = window.setTimeout(function () {
      var data = Smoke.delayedAjaxData[url];
      delete Smoke.delayedAjaxIds[url];
      delete Smoke.delayedAjaxData[url];
      Smoke.doAjax(url, data);
    }, time);
  };
  
  // xhr any ... -> void
  Smoke.onAjaxFailure = function (xhr) {
    Smoke.log("AJAX failed", arguments);
  
    alert("There was an error communicating with the server. "
      + "Try reloading the web page and contact your system administrator "
      + "if the problem persists.");
  };
  
  // DOM manipulation ============================
  
  //  element
  //  (U "replace" "children" "beforeBegin" "afterBegin" "beforeEnd" "afterEnd")
  //  string
  // ->
  //  void
  Smoke.insertHTML = function (anchor, where, html) {
    if (anchor.insertAdjacentHTML) {
      Smoke.ieInsertHTML(anchor, where, html);
    } else {
      // Safari and Firefox don't have insertAdjacentHTML, so we simulate
      // them with createRange and createContextualFragment:
      var range = anchor.ownerDocument.createRange();
      range.setStartBefore(anchor);
      var toInsert = range.createContextualFragment(html);
      Smoke.insertElement(anchor, where, toInsert);
    }
  };
  
  //  element
  //  (U "replace" "children" "beforeBegin" "afterBegin" "beforeEnd" "afterEnd")
  //  string
  // ->
  //  void
  //
  // Uses IE's element.insertAdjacentHTML method to add/replace HTML on the page.
  Smoke.ieInsertHTML = function (anchor, where, html) {
    if (where == "replace") {
      try {
        anchor.insertAdjacentHTML("afterEnd", html);
      } catch (e) {
        // HACK: IE messes up if you're trying to insert a table row or cell:
        var div = document.createElement("div");
        var tagName = anchor.tagName.toLowerCase();
        if (tagName == "tr") {
          div.innerHTML = "<table><tbody>" + html + "</tbody></table>";
          var newNode = div.childNodes[0].childNodes[0].childNodes[0];
        } else {
          div.innerHTML = "<table><tbody><tr>" + html + "</tr></tbody></table>";
          var newNode = div.childNodes[0].childNodes[0].childNodes[0].childNodes[0];
        }
        if (anchor.nextSibling) {
          anchor.parentNode.insertBefore(newNode, anchor.nextSibling);
        } else {
          anchor.parentNode.appendChild(newNode);                
        }
      }
      anchor.parentNode.removeChild(anchor);
    } else if (where == "children") {
      while (anchor.childNodes.length > 0) {
        anchor.removeChild(anchor.firstChild);
      }
      anchor.insertAdjacentHTML("beforeEnd", html);
    } else {
      anchor.insertAdjacentHTML(where, html);
    }
  };
  
  //  element
  //  (U "replace" "children" "beforeBegin" "afterBegin" "beforeEnd" "afterEnd")
  //  node
  // ->
  //  void
  Smoke.insertElement = function (anchor, where, toInsert) {
    if (where != "replace" && where != "children" && anchor.insertAdjacentElement) {
      anchor.insertAdjacentElement(where, toInsert);
    } else {
      switch (where) {
        case "beforeBegin":
          anchor.parentNode.insertBefore(toInsert, anchor)
          break;
        case "afterBegin":
          anchor.insertBefore(toInsert, anchor.firstChild);
          break;
        case "beforeEnd":
          anchor.appendChild(toInsert);
          break;
        case "afterEnd":
          if (anchor.nextSibling) {
            anchor.parentNode.insertBefore(toInsert, anchor.nextSibling);
          } else {
            anchor.parentNode.appendChild(toInsert);
          }
          break;
        case "replace":
          anchor.parentNode.replaceChild(toInsert, anchor);
          break;
        case "children":
          while (anchor.childNodes.length > 0) {
            anchor.removeChild(anchor.firstChild);
          }
          anchor.appendChild(toInsert);
          break;
      }
    }
  };
    
  // Delirium support ============================
  
  // hashOf(function -> function)
  //
  // A hash of wait hooks versus smoke-page-update event handlers.
  DeliriumClient.wrappers = {};
  
  // function function function -> void
  DeliriumClient.registerWaitHook = function (fn, unregister, log) {
    log("DeliriumClient", "register", fn);

    DeliriumClient.wrappers[fn] = function (evt) {
      unregister(fn);
      fn();
    };
    
    $(document).bind("smoke-page-update", DeliriumClient.wrappers[fn]);
  };
  
  // function -> void
  DeliriumClient.unregisterWaitHook = function (fn, log) {
    log("DeliriumClient", "unregister", fn);
    $(document).unbind("smoke-page-update", DeliriumClient.wrappers[fn]);
    delete DeliriumClient.wrappers[fn];
  };
  
})(jQuery);

