var clearError = function(){
  $('#error').text('');
};

var unauthorized = function(){
  $('#error').text('You must log in to edit the wiki.  Please log in or create a new account using the login form above.');
};

var showPreview = function(){
  $.ajax({
    url: "/preview", type: "POST", dataType: "json",
    data: { content: $('#editor').val() },
    success: function(data) {
      clearError();
      $('#previewContent').html(data.preview);
      $('#preview').show();
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to display preview.');
    }
  });
};

var save = function(){
  $.ajax({
    url: "/save", type: "POST", dataType: "json",
    data: {
      page: page,
      content: $('#editor').val(),
      revision: $('#revision').val(),
      comment: $('#comment').val()
    },
    success: function(args){
      clearError();
      window.location.href = '/wiki/view/'+page; //success
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
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

var showLoginView = function(){
  getUser(function(error, username){
    if(error) $('#error').text('Error retrieving user info');
    if(!username) showLoginForm();
    else showLogoutForm(username);
  });
};

var showLoginForm = function(){
  //not logged in, show login and signup form
  $('#username').removeClass('formerror'); //clear out any old error styling
  $('#password').removeClass('formerror');
  $('#loginForm').submit(doLogin);
  $('#signupForm').submit(doSignup);
  $('#signupButton').click(showSignup); //fixme: keypress?
  $('#loginView').show();
  $('#loggedInView').hide();
};

var showSignup = function(){
  $('#signupForm').dialog({});
};

var showLogoutForm = function(username){
  //user is logged in, show logout form and name
  $('#displayUserName').text(username);
  $('#loginView').hide();
  $('#logoutForm').submit(doLogout);
  $('#loggedInView').show();
};

var getUser = function(callback){
  $.ajax({
    url: "/getUser", type: "GET", dataType: "json",
    success: function(data){
      if(data.user)
        return callback(null, data.user);
      else //no user
        return callback(null, null);
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      return callback(errorThrown, null);
    }
  });
};

var validateLoginFormInput = function(usernameId, passwordId, emailId, errorId, callback) {
  var toValidate = { username: '#'+usernameId, password:'#'+passwordId };
  if(emailId) toValidate.email = '#'+emailId;
  var error;
  $.each(toValidate, function(label, selector){
    if($(selector).val().trim().length === 0){
      error = label;
      $(selector).addClass('formerror');
      $('#'+errorId).text('Please enter your '+label+'.');
      return false;
    }
    $(selector).removeClass('formerror');
  });
  if(error) return callback(error);
  return callback(null);
};

var handleLoginSuccess = function(data){
  clearError();
  showLogoutForm(data.user.name);
  redirectIfUnauthorized(data.path);
};

var doLogin = function(){
  validateLoginFormInput('username', 'password', null, 'error', function(err){
    if(err) return false;
    $.ajax({
      url: "/login", type: "POST", dataType: "json",
      data: {username: $('#username').val(), password: $('#password').val()},
      success: handleLoginSuccess,
      error: function(jqXHR, textStatus, errorThrown){
        console.log(errorThrown);
        $('#error').text('Login failed. Please try again.');
      }
    });
  });
  return false;
};

var redirectIfUnauthorized = function(path){
  if(window.location.pathname === '/unauthorized/'){
    window.location.href = path;
  }
};

var doSignup = function(){
  validateLoginFormInput('newusername', 'newpassword', 'newemail', 'formError', function(err){
    if(err) return false;
    $.ajax({
      url: "/signup", type: "POST", dataType: "json",
      data: {username: $('#newusername').val(), password: $('#newpassword').val()},
      success: function(data){
        handleLoginSuccess(data);
        $('#signupForm').dialog('close');
      },
      error: function(jqXHR, textStatus, errorThrown){
        console.log(errorThrown);
        $('#formError').text('Signup failed. Please try again. ' + jqXHR.responseText);
      }
    });
  });
  return false;
};

var doLogout = function(){
  $.ajax({
    url: "/logout", type: "POST",
    success: function(data){
      clearError();
      window.location.href = window.location.href;
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to log out.');
    }
  });
  return false;
};
