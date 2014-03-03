(function () {

  function pad(n, len) {
    n = n.toString(16);
    while (n.length < len) {
      n = "0" + n;
    }
    return n;
  }

  function safeJSON(obj) {
    obj = JSON.stringify(obj);
    return obj.replace(/([\u0080-\uffff])/g, function (m, c) {
      return "\\u" + pad(String.toCharCode(c), 4);
    });
  }

  function streamJSON(objs) {
    var s = [];
    for (var i=0; i<objs.length; i++) {
      var serialized = safeJSON(objs[i]);
      s.push(pad(serialized.length, 6));
      s.push(serialized);
    }
    return s.join("");
  }

  function parseJSON(data, offset) {
    var dataLength = data.length + (offset||0);
    var result = {
      objects: [],
      position: 0
    };
    while (true) {
      if (dataLength - result.position < 6) {
        return result;
      }
      var len = parseInt(data.substr(result.position, 6), 16);
      if (dataLength - result.position - 6 < len) {
        return result;
      }
      var obj = JSON.parse(data.substr(result.position+6, len));
      result.objects.push(obj);
      result.position += 6 + len;
    }
  }

  var RTDB = window.RTDB = function (options) {
    this.url = options.url;
    this.app = options.app;
    this.pollPeriod = options.pollPeriod || 30000;
    var state = JSON.parse(localStorage.getItem("rtdb." + this.url) || "{}");
    this.collection = state.collection || null;
    this.etag = state.etag || null;
    this.position = state.position || 0;
    this.scheduleTimeout = null;
  };

  RTDB.prototype = {
    storeState: function () {
      localStorage.setItem("rtdb." + this.url, JSON.stringify({
        collection: this.collection,
        etag: this.etag,
        position: this.position
      }));
    },

    resetState: function () {
      this.cancelSchedule();
      this.collection = null;
      this.etag = null;
      this.position = 0;
      this.app.resetSaved();
      this.storeState();
    },

    request: function (options) {
      var req = new XMLHttpRequest();
      console.log("REQUEST", options.method || "GET", options.headers, options.body);
      req.open(options.method || "GET", options.url || this.url);
      for (var a in (options.headers || {})) {
        req.setRequestHeader(a, options.headers[a]);
      }
      req.onreadystatechange = (function () {
        if (req.readyState == 4) {
          options.done.call(options.context || this, req);
        }
      }).bind(this);
      req.send(options.body || "");
    },

    get: function (onDone) {
      var headers = {};
      if (this.collection) {
        console.log("for collection", this.collection, "bytes", this.position, this.etag);
        headers["Range"] = "bytes " + this.position + "-";
        headers["If-None-Match"] = this.etag;
      }
      this.request({
        headers: headers,
        done: function (req) {
          console.log("got", req.status, req.responseText);
          if (req.status == 304) {
            this.app.incoming([]);
            onDone();
            return;
          }
          var data = req.responseText;
          var collection = data.substr(data.length-8, 8);
          if (! this.collection) {
            this.collection = collection;
          }
          if (req.status == 404 || collection != this.collection) {
            this.makeCollection(onDone);
            return;
          }
          var result = parseJSON(data, -8);
          this.app.incoming(result.objects);
          console.log("get result", result, this.position, this.position + result.position, this.etag, req.getResponseHeader("ETag"));
          this.position += result.position;
          this.etag = req.getResponseHeader("ETag");
          this.storeState();
          onDone();
        }
      });
    },

    makeCollection: function (onDone) {
      var collection = "C" + pad(Math.floor(Math.random() * 0xfffffff));
      this.request({
        body: collection,
        method: "PUT",
        done: function (req) {
          this.collection = collection;
          this.etag = req.getResponseHeader("ETag");
          this.position = 0;
          this.app.resetSaved();
          this.storeState();
          onDone();
        }
      });
    },

    put: function (onDone) {
      if (! this.collection) {
        onDone();
        return;
      }
      var objs = this.app.getPending((function (lazyObjects) {
        this.putObjects(lazyObjects, onDone);
      }).bind(this));
      if (objs !== undefined) {
        this.putObjects(objs, onDone);
      }
    },

    putObjects: function (objs, onDone) {
      if (! objs.length) {
        onDone();
        return;
      }
      var body = streamJSON(objs) + this.collection;
      this.request({
        body: body,
        method: "PUT",
        headers: {
          "Content-Range": "bytes " + this.position + "-" + (this.position + body.length - 1) + "/*",
          "If-Matches": this.etag
        },
        done: function (req) {
          if (req.status == 412) {
            // Precondition failed, try again
            this.poll(onDone);
            return;
          }
          this.app.markSaved(objs);
          this.position += body.length - 8;
          this.etag = req.getResponseHeader("etag");
          this.storeState();
          onDone();
        }
      });
    },

    poll: function (onDone) {
      this.get((function () {
        this.put(onDone);
      }).bind(this));
    },

    cancelSchedule: function () {
      clearTimeout(this.scheduleTimeout);
      this.scheduleTimeout = null;
    },

    schedule: function () {
      this.scheduleTimeout = setTimeout((function () {
        this.update(true);
      }).bind(this), this.pollPeriod);
    },

    update: function (getFirst) {
      this.cancelSchedule();
      var method = getFirst ? "poll" : "put";
      this[method](this.schedule.bind(this));
    }

  };

})();
