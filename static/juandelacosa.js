$(function() {
  var info = $('#info');
  var infoHead = $('#info>h1');
  var infoAlert = $('#info>div');
  var main = $('#main');
  var noSuchAccount = $('#noSuchAccount');
  var password = $('#password');
  var passwordMessage = $('#passwordMessage');
  var resetPassword = $('#resetPassword');

  document.title = window.location.hostname + ' - ' + 'Juan De La Cosa';

  (function whoAmI() {
    $.ajax({
      url: "whoAmI",
      method: "GET",
      success: function(login) {
        info.hide();
        main.show();
        $('#login').val(login);
        setTimeout(whoAmI, 60 * 1000);
      },
      error: function(jqXHR, textStatus, errorThrown) {
        main.hide();
        if ((404 == jqXHR.status) && ('' != jqXHR.responseText)) {
          infoHead.text('Your account does not exist');
          infoAlert.html('Your login would be <strong>' + jqXHR.responseText +
            '</strong>, but it does not exist in the database.');
          infoAlert.removeClass().addClass('alert alert-info');
          setTimeout(whoAmI, 60 * 1000);
        } else {
          infoHead.text('An error has occured');
          infoAlert.text((0 == jqXHR.readyState) ?
            'Service unavailable' : errorThrown);
          infoAlert.removeClass().addClass('alert alert-danger');
          setTimeout(whoAmI, 10 * 1000);
        }
        info.show();
      }
    })
  })();

  function showPasswordMessage(succ, msg, done) {
    if (succ) {
      passwordMessage.removeClass().addClass('alert alert-success');
    } else {
      passwordMessage.removeClass().addClass('alert alert-danger');
    }
    passwordMessage.fadeIn();
    passwordMessage.text(msg);
    setTimeout(function() {
      passwordMessage.fadeOut();
      if (typeof done == 'function') done();
    }, 5 * 1000);
  };

  resetPassword.click(function() {
    $.ajax({
      url: "resetMyPassword",
      method: "POST",
      error: function(jqXHR, textStatus, errorThrown) {
        resetPassword.prop('disabled', true);
        showPasswordMessage(false, (0 == jqXHR.readyState) ?
          'Service unavailable' : errorThrown,
          function() {
            resetPassword.prop('disabled', false)
          });
      },
      success: function(newpwd) {
        resetPassword.hide();
        password.val(newpwd);
        password.show();
        password.select();

        showPasswordMessage(true, 'Password changed.');

        setTimeout(function() {
          password.val('');
          password.hide();
          resetPassword.show();
        }, 10 * 1000);
      }
    });
  })
});
