
var friendsInterval;
     
function friendsStart() {
    
    // Set friend checking timer
    friendsInterval = setInterval(updateFriends, 4000);
    
    // Set the user information
    $.get('login', {}, function(data) {
	
	$(".email").html(data.email);
	$(".avatar").html("<img class=\"icon\" src=\"avatar?email=" + data.email + "\"></img>");
    }, "json");
}

function friendsStop() {
    clearInterval(friendsInterval);
}

function updateFriends() {
    $.get('friends', {}, function(data) {
	
	var text = "";
	
	for (key in data) {

            if (data[key]["listening"]["title"] === undefined) {
                var title = "";
            } else {
                var title = data[key]["listening"]["title"];
            }
	    
	    var friends_text = $('#contact').html().replace('xxkey', key).replace('xxkey', key)
            .replace(/title_key/g, title)
            .replace(/mp3_key/g, data[key]["listening"]["id"])
            .replace(/band_key/g, data[key]["listening"]["name"]);
	    
	    var messages_text = "";
	    
	    if (data[key]["messages"] != null) {
                for (msg in data[key]["messages"]) {
		    messages_text += '<p>' + data[key]["messages"][msg] + '</p>';
                }
                friends_text += $('#messages').html().replace(/xxkey/, messages_text);
	    }
	    
	    text += $('#contact_link').html().replace(/xxkey/, "contact_" + key).replace(/xxxxkey/, friends_text);
	}
	
	$("#friends_list").html(text);
	$("#friends_list a.recommend").hover(function() {
            $(this).parent().find("img.play").slideDown(200);},
		                             function() {
                                                 $(this).parent().find("img.play").slideUp(200);});
	
	$("#friends_list a.contact_link_a").hover(function() {
            $(this).find("img.msg").slideDown(200);},
		                                  function() {
                                                      $(this).find("img.msg").slideUp(200);});

        var top = $("#friends_list").css("top");
        var jtop = $("#friends_list_wrapper .jScrollPaneDrag").css("top");

	$("#friends_list").jScrollPaneRemove();
        $("#friends_list").jScrollPane().css("top", top);
        $("#friends_list_wrapper .jScrollPaneDrag").css("top", jtop);

	
    }, "json");
}
