
function cleanSlidy() {
	titlePage();
	addHeader();
	addFooter();
	removeToolbar();
}

function removeToolbar() {
	$("div.toolbar").hide();
}

function titlePage() {
	$(".titlepage").append("<div class='titleLogo'/>")
}
function addHeader() {
	$(".section h1").wrap("<div class='header'></div>");
}
function addFooter() {
	footerHtml = "\
<div class='footer'> \
	<div class='footerLeft'>CONFIDENTIAL &#169 2016 ~DO NOT DISTRIBUTE~<br>SomaLogic, Inc. &#8226 2945 Wilderness Place &#8226 Boulder, CO &#8226 80301</div> \
	<div class='footerRight'>  \
	<!-- <img src='logo.png'> --> \
	</div> \
</div>"
	$(".slide").append(footerHtml)
}

$(document).ready(cleanSlidy);
