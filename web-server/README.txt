This wrapper for the web server re-provides most of its useful services except:

- send/suspend/dispatch has been reworked to embed thunks rather than
  (request -> response) procedures;

- the current request is now stored in a thread cell, accessible via
  (current-request);

- send/suspend/dispatch alters the web frame stack to preserve the current frame
  after an AJAX request;

- the behaviour of web cells has been altered: the default values of a web cell
  is stored in the current frame rather than the root frame (allowing web cells
  to be created on the fly while handling requests).
