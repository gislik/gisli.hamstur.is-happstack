$_blogs_form()$
<script>
function t(x) {
	if (x < 10) x = "0"+x;
	return x;
}
jQuery(function() {
	var now = new Date();
//	\$("#blogdate").datepicker("setDate", now);
	\$("#blogdate").val(t(now.getDate())+"."+t(now.getMonth()+1)+"."+t(now.getYear()+1900));
	\$("#blogtime").val(t(now.getHours())+":"+t(now.getMinutes())+":"+t(now.getSeconds()));
});
</script>