<script type="text/javascript" src="/lib/tiny_mce/tiny_mce.js"></script> 
<script type="text/javascript"> 
tinyMCE.init({ 
	// General options 
	mode : "textareas", 
	theme : "advanced", 
	height: "400px",
	plugins : "pagebreak,style,layer,table,save,advhr,advimage,advlink,emotions,iespell,inlinepopups,insertdatetime,preview,media,searchreplace,print,contextmenu,paste,directionality,fullscreen,noneditable,visualchars,nonbreaking,xhtmlxtras,template,imagemanager,filemanager", 
	// Theme options theme_advanced_buttons1 : 	"save,newdocument,|,bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,|,styleselect,formatselect,fontselect,fontsizeselect", 
	theme_advanced_buttons2 : "cut,copy,paste,pastetext,pasteword,|,search,replace,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,anchor,image,cleanup,code,|,preview,|,forecolor,backcolor", 
	theme_advanced_buttons3 : "tablecontrols,|,hr,removeformat,visualaid,|,sub,sup,|,charmap,emotions,iespell,media,advhr,|,print,|,fullscreen", 
	theme_advanced_buttons4 : "insertlayer,moveforward,movebackward,absolute,|,styleprops,spellchecker,|,cite,abbr,acronym,del,ins,attribs,|,visualchars,nonbreaking,template,blockquote,pagebreak,|,insertfile,insertimage", 
	theme_advanced_toolbar_location : "top", 
	theme_advanced_toolbar_align : "left", 
	theme_advanced_statusbar_location : "bottom", 
	theme_advanced_resizing : true, 
	// Example content CSS (should be your site CSS) 
	content_css : "/css/style.css", 
	// Drop lists for link/image/media/template dialogs 
	template_external_list_url : "/lib/lists/template_list.js", 
	external_link_list_url : "/lib/lists/link_list.js", 
	external_image_list_url : "/lib/lists/image_list.js", 
	media_external_list_url : "/lib/lists/media_list.js", 
	// Replace values for the template plugin 
	//template_replace_values : { username : "Some User", staffid : "991234" } 
}); 
</script>
<script>
jQuery(function() {
	\$("#blogdate").datepicker({dateFormat: "dd.mm.yy"});
});
</script>
 <form method="post"> 
 <input type="text" class="blogtitle" name="title" value="$blog.title$"/>
 <br/>
 <input id="blogdate" style="width: 85px" class="inline" type="text" name="date" value="$blog.date$" />
 <input id="blogtime" style="width: 70px" class="inline" type="text" name="time" value="$blog.time$" />
 <input name="private" type="radio" value="1" class="inline" $if(blog.private)$checked="checked"$endif$/>Private
 <input name="private" type="radio" value="0" class="inline" $if(!blog.private)$checked="checked"$endif$/>Public
 <br/>
 <textarea id="blogbody" name="body">
 $blog.body$
 </textarea>
 <br/>
 <input type="submit"/>
 </form>