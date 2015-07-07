// Ident and secret should ideally be passed from a server for security purposes.
// If serverAuthentication is true then you should remove these two values.

// Insecure method
// var xirsysConnect = {
// 	secure : false,
// 	data : {
// 		domain : 'www.xirsys.com',
// 		application : 'default',
// 		room : 'default',
// 		ident : 'jerzilla',
// 		secret : '06727832-b20f-4cc0-a954-5bed3310033d'
// 	}
// };

var xirsysConnect = {
	secure : false,
	data : {
		domain : 'www.xirsys.com',
		application : 'default',
		room : 'default',
		ident : 'jerzilla',
		secret : '06727832-b20f-4cc0-a954-5bed3310033d'
	}
};

// Secure method
/*var xirsysConnect = {
	secure : true,
	server : '/getToken.php',
	info : {
		domain : 'www.xirsys.com',
		application : 'default',
		room : 'default'
	}
};*/

