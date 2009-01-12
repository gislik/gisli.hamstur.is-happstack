<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8"/>
<meta name="verify-v1" content="AgvlYeIC/dPDGWqeASTU5HvKpWVEYc7Qqtk6rj1m+DU=" />
<title>$title$ $title1$</title>
<link rel="shortcut icon" href="favicon.gif" type="image/x-icon"/>
<script type="text/javascript" src="http://www.google.com/jsapi?key=ABQIAAAAbFex_4il6vVsj9li8xTKghRi_j0U6kJrkFvY4-OX2XYmEAa76BSmsop4Tt2MNHNzAadTFnxSasEUFA"></script>
<!-- <script type="text/javascript" src="/lib/jquery-1.2.6.min.js"></script> -->
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js"></script>
<script type="text/javascript" src="/lib/jquery-ui-personalized-1.6rc2.min.js"></script>
<link rel="stylesheet" type="text/css" href="/css/style.css"/>
<link rel="stylesheet" type="text/css" href="/css/custom.css"/>
<link rel="stylesheet" href="/css/jquery-ui-themeroller.css"/>
<link rel="alternate" type="application/atom+xml" href="$feedUrl$" title="$title$"/>
<script type="text/javascript" src="/lib/script.js"></script><script type="text/javascript">
jQuery(function() {
	printRSSFeed("http://www.google.com/reader/public/atom/user/03578015985020084768/state/com.google/broadcast", "#sharedItems", 1, 
		function() {
			\$("#sharedItemsMore").show();
		});
	printTwitterFeed("http://twitter.com/statuses/user_timeline/gislik.atom", 
		"#twitterItems", 1, 
		function() {
			\$("#twitterItemsMore").show();
		});
	\$("a.proxyStatic[heading]").bind("ajax", staticHandler).click(id2url);	
	\$("a.proxyBlog[heading]").bind("ajax", blogHandler).click(id2url);
	\$("a.proxyAlbum[heading]").bind("ajax", albumHandler).click(id2url);
	\$("a.proxyPseudo").bind("ajax", pseudoHandler);
	\$("a.mailto").bind("click", mailHandler);
	var query = location.pathname.substring(1).split("/")[0];
	if (!query) query = location.search.substring(1);
	var heading = \$(".heading");
	if (query) {
		if (query != "blog")
			heading.show();
		query = query.split("/");
		var link = \$("#"+query[0]);
		if (link.attr("href")) {
			query.shift()
			link.triggerHandler("ajax", query);
		} else {
			loadWrapper("404.html");
		}
	} else if (location.pathname == "/") {
		heading.show();		
		loadWrapper("/home.html");		
	}
});
</script>
</head>
<body>

<div id="header">
  
  <h1><a href="/"><span class="orange">.:</span> Gísli Kristjánsson</a></h1>
  <h2>".... já"</h2>
  
  <ul id="nav">
    <li><a id="me" class="proxyStatic" heading="Um mig" href="http://google.com/notebook/public/01738234982158720746/BDSdwQwoQzPC91Psi">Ég</a></li>
    <li><a id="blog" class="proxyPseudo" href="/blog/">Blog</a></li>
    <li><a id="albums" class="proxyAlbum" heading="Myndir" href="http://picasaweb.google.com/data/feed/base/user/giskri?kind=album&alt=rss&hl=en_US&access=public">Myndir</a></li>
    <li><a id="projects" class="proxyStatic" heading="Verkefni" href="http://www.google.com/notebook/public/01738234982158720746/BDShxQwoQoIbXy_oi">Verkefni</a></li>
  </ul>
  
</div>

<div id="lang">
  <a id="english" href="http://www.google.com/notebook/public/01738234982158720746/BDRNDQgoQw9XVquYj" class="proxyStatic" heading="Welcome"><img src="/images/icons/usa.png" style="z-index:3" height="32" border="0"/></a>
</div>
  
<div id="container">
  
<div id="left">

  <h1 class="heading" id="heading" style="display: none">Velkomin á heimili mitt</h1>
  <h2 class="heading" style="display: none"><a class="mailto" href="mailto:hamstur.is!gislik">Gísli Kristjánsson</a> þann 24. desember 2008 &nbsp; </h2>
  <span id="content">$body$</span>
  &nbsp;
</div>

<div id="right">
  
  <h1>Um vefinn</h1>
  
  <p><img id="portrait" src="http://www.google.com/s2/photos/public/AIbEiAIAAABECNmn8sjy-4iN4QEiC3ZjYXJkX3Bob3RvKig4M2Y2OWFmYzI2NjExYzM0Zjc1YTA5YmUwNTYxZjhiYzE0OGMxNzI5MAEexe_-Qq4kcLlUejbEenRYBnyDQg" width="96" height="96"/><i>Þessi síða er heimili mitt á vefnum. Pælingin er að halda utan um allt efnið mitt á einum aðgengilegum stað.</i>
  </p>

  <p>  <a href="$feedUrl$?format=xml" rel="alternate" type="application/rss+xml"><img src="http://www.feedburner.com/fb/images/pub/feed-icon32x32.png" alt="" border="0" /></a>  <a href="http://www.facebook.com/people/Gisli-Kristjansson/622735970?"><img src="/images/icons/facebook.png" height="32" align="bottom" border="0"/></a>  <a href="http://www.youtube.com/user/gislikristjans"><img src="/images/icons/youtube.png" height="32" border="0"/></a>  <a href="http://twitter.com/gislik"><img src="/images/icons/twitter.png" height="32" border="0"/></a> </p> 
  <p>
  <a id="about" class="proxyStatic" heading="Um vefinn" href="http://www.google.com/notebook/public/01738234982158720746/BDSe5QgoQ0NzJ8vsi">Tæknilýsing á vefnum</a>
  </p>

  $if(!user)$
  <p>
  <a class="rpxnow" onclick="return false;" href="https://gisli-hamstur-is.rpxnow.com/openid/v2/signin?token_url=http://gisli.hamstur.is/users/token">Innskráning</a> 
  </p>
  $endif$

<div class="hr"></div>

$if(user)$
<h1>$user.name$</h1>

<p>
<a href="/blog/new">Nýtt blog</a><br/>
<a href="/sessions/">Setur</a><br/>
<a href="/users/logout">Skrá út</a><br/>
</p>

<div class="hr"></div>
$endif$

<h1>Þessa stundina</h1>

<ul id="twitterItems" class="feed"></ul>
<a id="twitterItemsMore" href="http://twitter.com/gislik" class="hidden">Twitter-síðan mín</a>

<div class="hr"></div>

<h1>Áhugavert</h1>

<ul id="sharedItems" class="feed snippet"></ul>
<a id="sharedItemsMore" href="http://www.google.com/reader/shared/03578015985020084768" class="hidden">Google-síðan mín</a>

<div class="hr"></div>

<h1>Tenglar</h1>

<ul class="links">
  <li><a href="http://haskell.org">Haskell</a></li>
  <li><a href="http://www.realworldhaskell.org/blog/">Real World Haskell</a></li>
  <li><a href="http://happs.org">HAppS</a></li>
  <li><a href="http://happstutorial.com">Real World HAppS</a></li>
  <li><a href="http://baldurkristjans.is">Baldur Kristjáns</a></li>
  <li><a href="http://bktimes.is">BK Times</a></li>
  <li><a href="http://darri.skuli.is">Gísli Darri</a></li>
</ul>

<div class="hr"></div>

</div>

<div id="clear"></div>

</div>


<div id="copyright">&copy;2008 Gísli Kristjánsson &nbsp; <a href="http://free-css-templates.gorotron.com">CSS template</a> by <a href="http://free-css-templates.gorotron.com">gorotron</a>.</div>

<script src="/lib/widget.js" type="text/javascript"></script>
<script type="text/javascript">
RPXNOW.token_url = "http://gisli.hamstur.is/users/token";

RPXNOW.realm = "gisli-hamstur-is";
RPXNOW.overlay = true;
</script>
<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
try {
var pageTracker = _gat._getTracker("UA-5464683-1");
pageTracker._trackPageview();
} catch(err) {}</script>
</body>
</html>
