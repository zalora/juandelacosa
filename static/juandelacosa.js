$(function () {
  var account = $('#account');
  var main = $('#main');
  var noSuchAccount = $('#noSuchAccount');
  var password = $('#password');
  var passwordChanged = $('#passwordChanged');
  var passwordFailed = $('#passwordFailed');
  var resetPassword = $('#resetPassword');

  document.title = window.location.hostname + ' - ' + 'Juan De La Cosa';

  (function whoAmI() {
    $.ajax({
      url: "whoAmI",
      method: "GET",
      success: function (login) {
        noSuchAccount.hide();
        main.show();
        $('#login').val(login);
      },
      error: function (jqXHR, textStatus, errorThrown) {
        if (404 == jqXHR.status) {
          main.hide();
          account.text(jqXHR.responseText);
          noSuchAccount.show();
        }
      },
      complete: setTimeout(whoAmI, 60 * 1000)
    })
  })();

  resetPassword.click(function () {
    $.ajax({
      url: "resetMyPassword",
      method: "POST",
      error: function (jqXHR, textStatus, errorThrown) {
        resetPassword.prop('disabled', true);
        passwordFailed.fadeIn();
        if (0 == jqXHR.readyState) {
          passwordFailed.text('Service unavailable');
        } else {
          passwordFailed.text(errorThrown);
        }
        setTimeout(function () {
          passwordFailed.fadeOut();
          resetPassword.prop('disabled', false);
        }, 5 * 1000);
      },
      success: function (newpwd) {
        resetPassword.hide();
        password.val(newpwd);
        password.show();
        passwordChanged.fadeIn();
        password.select();

        setTimeout(function () {
          passwordChanged.fadeOut();
        }, 5 * 1000);

        setTimeout(function () {
          password.val('');
          password.hide();
          resetPassword.show();
        }, 10 * 1000);
      }
    });
  })
});

