var todos;
var server;
var lists;
var listsServer;

$(function () {

  var listName = "main";
  var match = /list=([^&]*)/.exec(location.search);
  if (match) {
    listName = match[1];
  }
  $("#list-name").text(listName);

  lists = JSON.parse(localStorage.getItem("listnames") || "[]");
  addToLists(listName, false);

  function updateLists() {
    var $lists = $("#lists");
    $lists.empty();
    lists.forEach(function (name) {
      var $li = $('<li><a href=""></a></li>');
      $li.find("a").attr("href", "?list=" + name).text(name);
      $lists.append($li);
    });
  }

  function addToLists(name) {
    if (lists.indexOf(name) != -1) {
      return;
    }
    lists.push(name);
    lists.sort();
    updateLists();
    localStorage.setItem("listnames", JSON.stringify(lists));
  }

  updateLists();

  var $input = $("#input");
  var $todos = $("#todos");

  todos = JSON.parse(localStorage.getItem("list." + listName) || "{}");
  for (var id in todos) {
    update(todos[id]);
  }

  function update(item) {
    var $el = $("#" + item.id);
    if (item.deleted) {
      if ($el.length) {
        $el.remove();
      }
      return;
    }
    if (! $el.length) {
      $el = $('<li><input type=checkbox> <span class="name"></span></li>');
      $el.attr("id", item.id);
      $todos.append($el);
    }
    $el.find(".name").text(item.name);
    if (item.done) {
      $el.find("input").prop("checked", true);
    }
  }

  function save() {
    localStorage.setItem("list." + listName, JSON.stringify(todos));
  }

  $input.keypress(function (event) {
    if (event.which == 13 && $input.val()) {
      var todo = {
        id: "item-" + Date.now(),
        name: $input.val(),
        done: false,
        deleted: false
      };
      todos[todo.id] = todo;
      save();
      update(todo);
      $input.val("");
    }
  });

  $(document).bind("change", "input[type=checkbox]", function (event) {
    var todo = todos[$(event.target).closest("li").attr("id")];
    console.log("change on", event.target, todo);
    if (! $(event.target).prop("checked")) {
      todo.deleted = true;
    } else {
      todo.done = true;
    }
    update(todo);
    todo.saved = false;
    save();
  });

  server = new RTDB({
    url: "../../list-" + listName,
    app: {
      incoming: function (objs) {
        objs.forEach(function (todo) {
          todo.saved = true;
          todos[todo.id] = todo;
          update(todo);
          save();
        });
      },
      resetSaved: function () {
        for (var id in todos) {
          todos[id].saved = false;
        }
        save();
      },
      getPending: function () {
        var result = [];
        for (var id in todos) {
          if (! todos[id].saved) {
            result.push(todos[id]);
          }
        }
        return result;
      },
      markSaved: function (objs) {
        objs.forEach(function (todo) {
          todos[todo.id].saved = true;
        });
        save();
      }
    }
  });

  listsServer = new RTDB({
    url: "../../lists",
    app: {
      incoming: function (names) {
        names.forEach(function (name) {
          addToLists(name);
        });
        this.markSaved(names);
      },
      resetSaved: function () {
        localStorage.setItem("listnames.saved", "");
      },
      getPending: function () {
        var saved = JSON.parse(localStorage.getItem("listnames.saved") || "[]");
        return lists.filter(function (name) {
          return saved.indexOf(name) == -1;
        });
      },
      markSaved: function (names) {
        localStorage.setItem("listnames.saved", JSON.stringify(
          JSON.parse(localStorage.getItem("listnames.saved") || "[]").concat(names)));
      }
    }
  });


  server.schedule();

});
