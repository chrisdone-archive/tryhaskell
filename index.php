<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
          "http://www.w3.org/TR/html4/strict.dtd">
<html>
  <head>
    <title>Try Haskell! An interactive tutorial in your browser</title>
    <meta name="Content-Type" content="text/html; charset=UTF-8">
    <meta name="google-site-verification"
          content="q3BH34YRyNZdPysYNE0FbM4bJl2hkY67BK0tEExs1a4" />
    <meta name="description" content="Try Haskell in your browser! An interactive tutorial for the Haskell programming language." />
    <link rel="stylesheet" href="css/reset.css" type="text/css"
          media="screen">
    <link rel="stylesheet" href="css/fontsquirrel.css" type="text/css"
          media="screen">
    <link rel="stylesheet" href="css/tryhaskell.css" type="text/css"
          media="screen">
    <!--[if lte IE 6]>
    <link rel="stylesheet" href="css/tryhaskell.ie6.css" type="text/css"
          media="screen">
    <![endif]-->
    <script type="text/javascript" src="js/jquery-1.4.2.min.js"></script>
    <script type="text/javascript" src="js/jquery.curvycorners.min.js"></script>
    <script type="text/javascript" src="js/jquery.base64.js"></script>
    <script type="text/javascript" src="js/jquery.console.js"></script>
    <script type="text/javascript" src="js/json2.js"></script>
    <script type="text/javascript" src="js/raphael.min.js"></script>
    <script type="text/javascript" src="js/tryhaskell.js"></script>
  </head>
  <body>
    <div class="badge-wrap"><div class="badge"></div></div>
    <div class="page-wrap">
      <div class="primary-content">
        <div class="main-wrapper">
          <div class="main-wrapper-top"></div>
          <div class="main-wrapper-borders">
            <h1>Try Haskell! An interactive tutorial in your
              browser</h1>
            <div class="menu clearfix">
              <!--
                 <a class="editor-btn" href="javascript:">
                   <span>Editor</span>
                 </a>
                 -->
              <a class="reset-btn" href="javascript:">
                <span>Reset</span>
              </a>
            </div>
            <div class="clear"></div>
            <div class="console-wrapper">
              <div class="console-wrapper-top"></div>
              <div class="console-wrapper-borders">
                <div class="console">
                </div>
              </div>
              <div class="console-wrapper-bottom"></div>
            </div>
            <!--
               <div class="editor-wrapper">
                 <div class="editor-wrapper-top"></div>
                 <div class="editor-wrapper-borders">
                   <div class="editor">
                     <textarea rows="10" cols="20">
                       Test.
                     </textarea>
                   </div>
                 </div>
                 <div class="editor-wrapper-bottom"></div>
               </div>
               -->
            <div class="guide-wrapper" style="margin-bottom:10px">
              <div class="guide-wrapper-top"></div>
              <div class="guide-wrapper-borders">
                <div id="raphael">
                </div>
              </div>
              <div class="guide-wrapper-bottom"></div>
            </div>
            <div class="guide-wrapper">
              <div class="guide-wrapper-top"></div>
              <div class="guide-wrapper-borders">
                <div class="guide">
                  <p>
                    Welcome to your first taste of Haskell! Let's try Haskell right now!
                  </p>
                  <h3>Beginners</h3>
                  <div class="indent">
                    <p>Type <code>help</code> to start the tutorial.</p>
                    <p>Or try typing these out and see what happens:</p>
                    <ul>
                      <li>
                        <code>23*36</code>
                      </li>
                      <li>
                        <code>reverse "hello"</code>
                      </li>
                    </ul>
                  </div>
                  <h3>Learn More</h3>
                  <div class="indent">
                    <p class="clearfix">
                      <a href="http://book.realworldhaskell.org/"
                         title="Real World Haskell">
                        <img alt="Real World Haskell book cover"
                             class="book-thumb"
                             src="images/rwh-thumb.gif"></a>
                      Get stuck into a book with
                      <a href="http://book.realworldhaskell.org/">Real
                        World Haskell</a> <small class="note">(readable
                        online!)</small>, published by O'Reilly Media.
                      Checkout
                      <a href="http://haskell.org/"
                         title="Haskell home page">Haskell.org</a> 
                      for more information about Haskell.</p>
                  </div>
                </div>
              </div>
              <div class="guide-wrapper-bottom"></div>
            </div>
          </div>
          <div class="main-wrapper-bottom"></div>
        </div>
        <div class="share-wrapper">
          <p>
            <span title="Share on Facebook">
              <a title="Bookmark with Facebook" href="http://www.facebook.com/sharer.php?u=http://tryhaskell.org/">
                <img width="16" height="16" src="images/facebook.png" alt="Bookmark with Facebook"/>
              </a>
            </span>
            <span title="Share on Digg">
              <a title="Share on Digg.com" href="http://digg.com/submit?url=http://tryhaskell.org/">
                <img width="16" height="14" src="images/digg.png" alt="Share on Digg.com"/>
              </a>
            </span>
            <span title="Share on delicious">
              <a title="Share on delicious" href="http://delicious.com/post?url=http://tryhaskell.org/">
                <img width="18" height="18" src="images/delicious.png" alt="Share on delicious"/>
              </a>
            </span>
            <span title="Share on reddit">
              <a title="Share on reddit.com" href="http://reddit.com/submit?url=http://tryhaskell.org/">
                <img width="18" height="18" src="images/reddit.png" alt="Share on reddit.com"/>
              </a>
            </span>
            <span title="Share on StumbleUpon">
              <a title="Share on stumbleupon.com" href="http://stumbleupon.com/submit?url=http://tryhaskell.org/">
                <img width="18" height="16" src="images/stumbleupon.png" alt="Share on stumbleupon.com"/>
              </a>
            </span>
            <span title="Share on Technorati">
              <a title="Share on Technorati" href="http://www.technorati.com/faves?add=http://tryhaskell.org/">
                <img width="16" height="15" src="images/technorati.png" alt="Share on Technorati"/>
              </a>
            </span>
            <span title="Share on Twitter">
              <a title="Share on twitter.com"
                 href="http://twitter.com/?status=try+haskell+in+your+browser!+http://tryhaskell.org/">
                <img width="18" height="18" src="images/twitter.png" alt="Share on twitter.com"/>
              </a>
            </span>
            <span title="Share on Identi.ca">
              <a title="Share on Identi.ca" href="http://identi.ca/notice/new?status_textarea=try+haskell+in+your+browser!+http://tryhaskell.org/">
                <img width="16" height="16" src="images/identica.png" alt="Share on Identi.ca"/>
              </a>
            </span>
            <span title="Share on Hyves">
              <a title="Share on Hyves" href="http://www.hyves.nl/profielbeheer/toevoegen/tips/?name=Try+Haskell!&amp;text=try+haskell+in+your+browser!+http://tryhaskell.org/">
                <img width="16" height="16" src="images/hyves.png" alt="Share on Hyves"/>
              </a>
            </span>
          </p>
        </div>
        <div class="footer-wrapper clearfix">
          <div class="footer-wrapper-top"></div>
          <div class="footer-wrapper-borders">
            <div class="footer">
              <p>
                Snerged by
                <a href="http://chrisdone.com/">
                  Chris Done,
                </a>
                concept and interface blatantly copied from
                <a href="http://tryruby.org/" title="_why's Try
                                                     Ruby">
                  _why's Try Ruby!,
                </a>
                Haskell evaluator powered by a patched version of 
                <a href="http://hackage.haskell.org/package/mueval"
                   title="mueval on Hackage">
                  Gwern Branwen's mueval,
                </a>
                console proudly written 
                in
                <a href="http://jquery.com/">
                  JQuery.
                </a>
              </p>
            </div>
          </div>
          <div class="footer-wrapper-bottom"></div>
        </div>
      </div>
    </div>
    <!--[if lte IE 6]>
    <script type="text/javascript" src="js/supersleight-min.js"></script>
    <![endif]-->
    <? if ($_SERVER['HTTP_HOST'] != "kiboki" && $_SERVER['HTTP_HOST'] != "kiboki.net") { ?>
    <script type="text/javascript">
      var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
      document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
    </script>
    <script type="text/javascript">
      try {
      var pageTracker = _gat._getTracker("UA-7443395-9");
      pageTracker._setDomainName("none");
      pageTracker._setAllowLinker(true);
      pageTracker._trackPageview();
      } catch(err) {}</script>
    <? } ?>
  </body>
</html>
