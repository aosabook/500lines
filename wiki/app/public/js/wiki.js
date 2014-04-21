var unauthorized = function(){
  $('#error').text('You must log in to edit the wiki.  Please log in or create a new account using the login form above.');
};

var showLoginView = function(){
  $.ajax({
    url: "/getUser", type: "GET", dataType: "json",
    success: function(data){
      if(data.user) showLogoutForm(data.user);
      else showLoginForm();
    },
    error: function(jqXHR, textStatus, errorThrown){
      $('#error').text('Error retrieving user info');
    }
  });
};

var showLoginForm = function(){
  $('#loginView').show();
  $('#loggedInView').hide();
};

var showSignup = function(){
  $('#signupForm').dialog({});
};

var showLogoutForm = function(username){
  $('#displayUserName').text(username);
  $('#loginView').hide();
  $('#loggedInView').show();
};

var handleLoginSuccess = function(data){
  $('#error').text('');
  showLogoutForm(data.user.name);
  redirectIfUnauthorized(data.path);
};

var redirectIfUnauthorized = function(path){
  if(window.location.pathname === '/unauthorized/'){
    window.location.href = path;
  }
};

var doLogin = function(){
  $.ajax({
    url: "/login", type: "POST", dataType: "json",
    data: {username: $('#username').val(), password: $('#password').val()},
    success: handleLoginSuccess,
    error: function(jqXHR, textStatus, errorThrown){
      $('#error').text('Login failed. Please try again.');
    }
  }); return false;
};

var doSignup = function(){
  $.ajax({
    url: "/signup", type: "POST", dataType: "json",
    data: {username: $('#newusername').val(), password: $('#newpassword').val()},
    success: function(data){
      handleLoginSuccess(data);
      $('#signupForm').dialog('close');
    },
    error: function(jqXHR, textStatus, errorThrown){
      $('#formError').text('Signup failed. Please try again. ' + jqXHR.responseText);
    }
  }); return false;
};

var doLogout = function(){
  $.ajax({
    url: "/logout", type: "POST",
    success: function(data){
      window.location.href = window.location.href;
    },
    error: function(jqXHR, textStatus, errorThrown){
      $('#error').text('Unable to log out.');
    }
  }); return false;
};

var showPreview = function(){
  $.ajax({
    url: "/preview", type: "POST", dataType: "json",
    data: { content: $('#editor').val() },
    success: function(data) {
      $('#previewContent').html(data.preview);
      $('#preview').show();
    },
    error: function(jqXHR, textStatus, errorThrown){
      $('#error').text('Unable to display preview.');
    }
  });
};

var save = function(){
  $.ajax({
    url: "/save", type: "POST", dataType: "json",
    data: { page: page, content: $('#editor').val(), revision: $('#revision').val(), comment: $('#comment').val() },
    success: function(){
      window.location.href = '/wiki/'+page; //success
    },
    error: function(jqXHR, textStatus, errorThrown){
      $('#error').text('Unable to save edits.');
      if(jqXHR.status === 409 && jqXHR.responseJSON){ //conflict
        var args = jqXHR.responseJSON;
        $('#error').text('Conflict Detected');
        $('#editView').addClass("left");
        $('#compareTitle').text('Saved '+ args.comparedate + ' by ' + args.username);
        $('#comparecontent').html(args.comparecontent);
        $('#comparecomment').text(args.comparecomment);
        $('#revision').val(args.revision); //use new revision on next save
        $('#compareView').show();
      }
    }
  });
};
