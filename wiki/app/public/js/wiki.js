var clearError = function(){
  $('#error').text('');
};

var listWikiPages = function(){
  $('#listView').show();
  $.ajax({
    url: "/listWikiPages",
    type: "GET",
    dataType: "json",
    success: function(data) {
      clearError();
      $.each(data.pages, function(index, page){
        $('#pages').append('<a href="/wiki/view?page='+page.id+'">'+page.id+'</a><br/>');
      });
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to list pages.');
    }
  });
};

var viewPage = function(){
  $.ajax({
    url: "/getWikiContent",
    type: "GET",
    dataType: "json",
    data: {page: page},
    cache: false,
    timeout: 5000,
    success: function(data) {
      clearError();
      var doc = data.doc;
      if(doc){
        $('#pageContent').append(data.doc.content);
        $('#pageMeta').append('Last edited by '+doc.user+' at '+doc.updatedDate +' - ' + doc.comment);
        $('#pageView').show();
      }
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to display page.');
    }
  });
};

var unauthorized = function(){
  $('#error').text('You must log in to edit the wiki.  Please log in or create a new account using the login form above.');
};

var showPreview = function(){
  $.ajax({
    url: "/preview",
    type: "POST",
    dataType: "json",
    data: {content: $('#editor').val()},
    cache: false,
    timeout: 5000,
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
    url: "/save",
    type: "POST",
    dataType: "json",
    data: {
      page: page,
      content: $('#editor').val(),
      revision: $('#revision').val(),
      comment: $('#comment').val()
    },
    success: function(args){
      clearError();
      window.location.href = '/wiki/view?page='+page; //success     //fixme: keep static routes for list/view/edit or combine into one?
    },
    cache: false,
    timeout: 5000,
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to save edits.');
      if(jqXHR.status === 409 && jqXHR.responseJSON){
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

var showEditPage = function(){
  $.ajax({
    url: "/getWikiContent",
    type: "GET",
    dataType: "json",
    data: {page: page, noHtml: true},
    cache: false,
    timeout: 5000,
    success: function(data) {
      clearError();
      var doc = data.doc;
      $('#revision').val(doc._rev);
      $('#editor').val(doc.content);
      $('#previewButton').click(showPreview);
      $('#saveButton').click(save);
      $('#editView').show();
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to display editor.');
    }
  });
};

var showLoginView = function(){
  getUser(function(error, username){
    if(error) $('#error').text('Error retrieving user info');
    else clearError();
    if(!username){
      showLoginForm();
    }else{
      showLogoutForm(username);
    }
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

var showLogoutForm = function(username){
  //user is logged in, show logout form and name
  $('#displayUserName').text(username);
  $('#loginView').hide();
  $('#logoutForm').submit(doLogout);
  $('#loggedInView').show();
};

var getUser = function(callback){
  $.ajax({
    url: "/getUser",
    type: "GET",
    dataType: "json",
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

var validateLoginFormInput = function(usernameId, passwordId, emailId, callback) {
  var userError;
  var passwordError;
  if($('#username').val().trim().length === 0){
    $('#username').addClass('formerror');
    userError = 'Please enter your username to log in. ';
  }else{
    $('#username').removeClass('formerror');
  }
  if($('#password').val().trim().length === 0){
    $('#password').addClass('formerror');
    passwordError = 'Please enter your password to log in. ';
  }else{
    $('#password').removeClass('formerror');
  }
  if(userError || passwordError){
    var errorMsg = '';
    if(userError) errorMsg += userError;
    if(passwordError) errorMsg += passwordError;
    $('#error').text(errorMsg);
    return callback(new Error(errorMsg));
  }else{
    return callback(null);
  }
};

var doLogin = function(){
  validateLoginFormInput('username', 'password', null, function(err){
    if(err) return false;
    $.ajax({
      url: "/login",
      type: "POST",
      dataType: "json",
      data: {username: $('#username').val(), password: $('#password').val()},
      success: function(data){
        clearError();
        showLogoutForm(data.user.name);
      },
      error: function(jqXHR, textStatus, errorThrown){
        console.log(errorThrown);
        $('#error').text('Login failed. Please try again.');
      }
    });
  });
  return false;
};

var doSignup = function(){
  validateLoginFormInput('newusername', 'newpassword', 'newemail', function(err){
    if(err) return false;
    $.ajax({
      url: "/signup",
      type: "POST",
      dataType: "json",
      data: {username: $('#newusername').val(), password: $('#newpassword').val()},
      success: function(data){
        clearError();
        showLogoutForm(data.user.name);
        hideSignup();
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
    url: "/logout",
    type: "POST",
    success: function(data){
      clearError();
      showLoginForm();
    },
    error: function(jqXHR, textStatus, errorThrown){
      console.log(errorThrown);
      $('#error').text('Unable to log out.');
    }
  });
  return false;
};

var showSignup = function(){
  $('#signupForm').dialog({});
};

var hideSignup = function(){
  $('#signupForm').dialog('close');
};
