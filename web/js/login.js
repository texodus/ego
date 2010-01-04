/**
     * Login Method
     */
function login(fn) {

    $("#login_box").fadeOut(500, function() {
        $("#app_box").fadeIn(500, function() {
            $("#browser").jScrollPaneRemove();
            $("#browser").jScrollPane();
        });
    })
    


    fn();

}

/**
     * Logout Method
     */
function logout(fn) {

    $("#app_box").fadeOut(500, function() {
        $("#login_box").fadeIn(500);
    });

    fn();
}

/**
     * Login Click
     */
function loginClick(fn) {

    $.post('login', { name: $('#name').val(),  password: $('#password').val() },
           function(data) {
               if (data.status == "success") {
                   
                   // Login Succeeded
                   login(fn);
               } else {
                   
                   // Login Failed
                   $(".error").html("<br/>Login Failed").slideDown();
                   
                   setTimeout(function() {
                       $(".error").slideUp();
                   }, 2000);
               }
           }, "json");
}

function logoutClick(fn) {
    $.get('logout', {},
          function() {
              logout(fn);
          });
};
