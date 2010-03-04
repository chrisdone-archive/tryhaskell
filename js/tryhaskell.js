// Try Haskell 1.0.1
// Tue Feb 23 18:34:48 GMT 2010
//
// Copyright 2010 Chris Done. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//    1. Redistributions of source code must retain the above
//       copyright notice, this list of conditions and the following
//       disclaimer.

//    2. Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials
//       provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY CHRIS DONE ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL CHRIS DONE OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
// OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
// BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
// USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.

// The views and conclusions contained in the software and
// documentation are those of the authors and should not be
// interpreted as representing official policies, either expressed or
// implied, of Chris Done.
//
// TESTED ON
//   Internet Explorer 6
//   Opera 10.01
//   Chromium 4.0.237.0 (Ubuntu build 31094)
//   Firefox 3.5.8

(function($){
/*
    var raphaelPaper;
    var raphaelObjs;
    */
    var tutorialGuide;
    // Page variables
    //
    var nemesis = 'chirs';
    var pages =
        [
            ////////////////////////////////////////////////////////////////////////
            // Lesson 1

            // Simple addition
            {guide:
             '<h3>' + rmsg(['Learning By Numbers','Music is Math','Back to Basics'])
             + '</h3>' 
             + "<p>To kick off let's try some maths out. Up there you can"
             + " type in Haskell expressions. Try this out: <code>5 + 7</code></p>"
            },
            {guide:function(result){
                if (!result) result = {expr:'5+7',result:12};
                var complied = result.expr.replace(/ /g,'')=="5+7";
                var who = complied? 'we' : 'you';
                return '<h3>' + rmsg(['Your first Haskell expression',
                                      "First Time's a Charm"]) + '</h3>' 
                    + '<p>Well done, you typed it perfect! You got back the number'+
                    ' <code>' + result.result + '</code>. Just what '+who+' wanted. '
                    + "</p><p>Let's try something completely different."+
                    " Type in your name like this:" +
                    ' <code>"chris"</code></p>'
            },
             trigger:function(result){
                 return result.type == "(Num t) => t" ||
                     result.type == "Integer" ||
                     result.type == "Int";
             }
            },
            // Strings & types
            {guide:function(result){
                if (!result) result = {expr:'"chris"',result:"\"chris\""};
                var n = unString(result.result); if (n) n = ", " +n;
                n += "!";
                return '<h3>' + rmsg(['Types of values',"What's in a name?"]) +
                    '</h3>' 
                    + '<p>Hi there' + htmlEncode(n)
                    + (n!="!"? " That's a pretty name. Honest." : "")
                    + " You're getting the hang of this! </p>"
                    + "<p>Each time, we're getting back the value and the type. So "+
                    "far, just a number and a list of characters.</p>" +
                    "<p>You can have lists of other stuff, too. Let's see your " +
                    " lottery numbers: <code>[42,13,22]</code></p>"
            },
             trigger:function(result){
                 return result.type == "[Char]"
                     || result.type == "String";
             }
            },
            // Overview of lesson 1
            {guide:function(result){
                if (!result) result = {result:"[42,13,22]"};
                return '<h3>' + rmsg(["Lesson 1 done already!"]) +
                    '</h3>' +
                    "<p>Great, you made a list of numbers! If you win we'll split" +
                    " the winnings, right?</p>" +
                    "<p>Let's see what you've learned so far:</p>" +
                    "<ol>"+
                    "<li>How to write maths and lists of things.</li>"+
                    "<li>In Haskell every value has a type.</li>"
                    +"</ol>" +
                    "<p>We can do stuff with lists. Maybe you want the lottery "+
                    "numbers sorted in the right order, try this: " +
                    "<code>sort " + result.result + "</code></p>"
            },
             trigger:function(result){
                 return result.type == "(Num t) => [t]";
             }
            },
            ////////////////////////////////////////////////////////////////////////
            // Lesson 2 - Functions
            // Functions on lists
            {guide:function(result){
                if (!result) result = {result:"[13,23,30]"};
                return '<h3>' + rmsg(["We put the funk in function"]) +
                    '</h3>' +
                    "<p>Congratulations, you just used a <strong>function</strong>."+
                    " They're how you get things done in Haskell." +
                    "<p>As you might've guessed, we got back <code>" +
                    htmlEncode(result.result)
                    + "</code>.</p><p>Ever wanted an evil twin nemesis? Me too. "+
                    "Luckily, we can sort lists of characters, or "+
                    "<strong>strings</strong>" +
                    ", in the same way as numbers! <code>sort \"chris\"</code></p>"
            },
             trigger:function(result){
                 return result.type == "(Num t, Ord t) => [t]";
             }
            },
            // Tuples
            {guide:function(result){
                if (!result) result = {result:"\"chirs\""};
                nemesis = htmlEncode(unString(result.result));
                return '<h3>' +
                    rmsg(["Tuples, because sometimes one value ain't enough!"]) +
                    '</h3>' +
                    "<p>Watch out for "+nemesis+"! " +
                    "They've got a ray gun, a dinosaur museum pamphlet and some " +
                    " butter, and <strong>they're out to get you!</strong></p>" +
                    "<p>We should keep our nemesis's credentials for the police." +
                    " My nemesis is 28 years of age: "+
                    "<code>(28,\"chirs\")</code></p>"
            },
             trigger:function(result){
                 return result.type == "[Char]";
             }
            },
            // Functions on tuples
            {guide:function(result){
                if (!result) result = {result:"(28,\"chirs\")"};
                var age = result.result.match(/^\(([0-9]+)+/);
                var villain = htmlEncode(result.result.replace(/\\"/g,'"'));
                return '<h3>' +
                    rmsg(["We'll keep them safe, sir.","Let it be"]) +
                    '</h3>' +
                    "<p>Is "+(age?age[1]:"that")+" a normal age for a " +
                    "super-villain?</p>" +
                    "<p>Actually, let's say our villain <em>is</em> " + 
                    villain +
                    ", how do we get their age?</p>" +
                    "<code>let villain = " + villain + " in fst villain</code>"
            },
             trigger:function(result){
                 return result.type == "(Num t) => (t, [Char])";
             }
            },
            // Summary of lesson 2
            {guide:function(result){
                return '<h3>' +
                    rmsg(["Lesson 2 done! Wow, great job!",
                          "Lesson 2 completo!"]) +
                    '</h3>' +
                    "<p>Good job! You got the age back from the tuple! Didn't " +
                    " even break a sweat, did you?</p>" +
                    "<p>Time to take a rest and see what you learned:</p>" +
                    "<ol>"+
                    "<li>Functions can be used on lists of any type.</li>" +
                    "<li>We can stuff values into tuples.</li>" + 
                    "<li>Getting the values back from tuples is easy.</li>"+
                    "</ol>"+
                    "<p>Next, we take a short detour to learn about " +
                    "<strong>syntactic sugar</strong>. " +
                    "Try typing this out:</p>" +
                    "<code>'a' : []</code>"
            },
             trigger:function(result){
                 return result.type == "(Num t) => t";
             }
            },
            // Lesson 3: Syntactic sugar 
            {guide:function(result){
                return '<h3>' +
                    rmsg(["You constructed a list!"]) +
                    '</h3>' +
                    "<p>Well done, that was tricky syntax. We used the (:) " + 
                    "function. It takes two values, some value and a list, and " +
                    " constructs a new list" +
                    " out of them. We call it 'cons' for short.</p>" +
                    "<p><code>'a'</code> is " +
                    "the character 'a', <code>[]</code> is an empty list. So " +
                    "tacking <code>'a'</code> at the start of an empty list just "+
                    "makes a list <code>['a']</code>!</p>" +
                    "<p>But thankfully we don't have to type out " +
                    "<code>'a' : 'b' : []</code> every time to we want to make a "+
                    "list of characters; we can use " +
                    "<strong>syntactic sugar</strong> and just write"+
                    " <code>['a','b']</code>. Don't believe me, check this!</p>" +
                    "<code>'a' : 'b' : [] == ['a','b']</code>"
            },
             trigger:function(result){
                 return result.type == "[Char]";
             }
            },
            // Booleans and string syntactic sugar
            {guide:function(result){
                return '<h3>' +
                    rmsg(["You're on fire!"]) +
                    '</h3>' +
                    "<p>You're handling this syntax really well, nice!</p>" +
                    "<p>We just got a boolean value back, and it said " +
                    "<code>True</code>. That means they're equal!</p>" +
                    "<p>One final demonstration on syntactic sugar for now:</p>" +
                    "<code>['a','b','c'] == \"abc\"</code>"
            },
             trigger:function(result){
                 return result.type == "Bool" &&
                     result.expr.replace(/[^':\[\]\=,]/g,'') == "'':'':[]==['','']";
             }
            },
            // Summary of syntactic sugar section
            {guide:function(result){
                return '<h3>' +
                    rmsg(["Lesson 3 over! Syntactic sugar is sweet"]) +
                    '</h3>' +
                    "<p>Let's have a gander at what you learned:</p>" +
                    "<ol>" +
                    "<li>In <code>'a' : []</code>, <code>:</code> is really just " +
                    " another function, just clever looking.</li>" + 
                    "<li>Pretty functions like this are written like (:) when " +
                    " you talk about them.</li>" +
                    "<li>A list of characters ['a','b'] can just be written " +
                    "\"ab\". Much easier!</li>"
                    + "</ol>" +
                    "<p>Phew! You're getting pretty deep! Your arch nemesis, " + 
                    nemesis + ", is gonna try to steal your " + rmsg(['mojo',
                                                                      'pizza']) + 
                    "! Let's learn a bit more about functions and passing " +
                    "them around <strong>-- coming soon.</strong></p>"
            },
             trigger:function(result){
                 return result.type == "Bool";
             }
            }

        ];
    var pageTrigger = -1;

    /*
      Some demo values:

      "<p>That's all for now! Here are some <a href=\"http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html\">neat things</a> you can "+
      "try:</p>"+
      "<ul><li><code>:t show</code></li>"+
      "<li><code>show 1</code></li>"+
      "<li><code>map (*5) [12,13,74]</code></li>"+
      "<li><code>reverse (map (+2) [1..5])</code></li>"+
      "<li><code>\"hello \" ++ \"world!\"</code></li>"+
      "<li><code>circle 20 30 20</code></li>"+
      "<li><code>mapM_ (\\x -> circl­e (10*x) (sin x*10+20) 10) [1..10]</code></li>"+
      "</ul>"
    */

    ////////////////////////////////////////////////////////////////////////
    // Unshow a string
    function unString(str){
        return str.replace(/^"(.*)"$/,"$1").replace(/\\"/,'"');
    }

    ////////////////////////////////////////////////////////////////////////
    // Random message from a list of messages    
    function rmsg(choices) {
        return choices[Math.floor((Math.random()*100) % choices.length)];
    }

    // Simple HTML encoding
    // Simply replace '<', '>' and '&'
    // TODO: Use jQuery's .html() trick, or grab a proper, fast
    // HTML encoder.
    function htmlEncode(text){
        var wbr = $.browser.opera? '&#8203;' : '';
        return (
            text.replace(/&/g,'&amp;')
                .replace(/</g,'&lt;')
                .replace(/</g,'&lt;')
                .replace(/ /g,'&nbsp;')
                .replace(/([^<>&]{10})/g,'$1<wbr>&shy;' + wbr)
        );
    };

    $(document).ready(function(){
        $('.reset-btn').click(function(){
            controller.reset();
            tutorialGuide.animate({opacity:0,height:0},'fast',function(){
                tutorialGuide.html(initalGuide);
                tutorialGuide.css({height:'auto'});
                tutorialGuide.animate({opacity:1},'fast');
            });

        });

        ////////////////////////////////////////////////////////////////////////
        // Raphael globals
        // Create Raphael canvas
        // $('#raphael').css('height','150px').parent().parent().hide();
        // raphaelPaper = Raphael($('#raphael')[0],536, 150);
        // raphaelObjs = {};

        ////////////////////////////////////////////////////////////////////////
        // Guide globals
        // Get the guide element.
        tutorialGuide = $('.guide');
        var initalGuide = tutorialGuide.html();

        ////////////////////////////////////////////////////////////////////////
        // Create console
        var console = $('.console');
        var controller; // Console controller
        if (!$.browser.msie) {
            var loader = $('<img src="images/ajax-loader.gif">');
            console.parent().append(loader);
            console.hide();
        }
        setTimeout(function(){
            if (!$.browser.msie) {
                loader.remove();
                console.slideDown();
            }
            controller = console.console({
                promptLabel: '> ',
                commandValidate:function(line){
                    if (line == "") return false; // Empty line is invalid
                    else return true;
                },
                commandHandle:function(line,report){
                    if (libTrigger(line,report)) return;
                    // TODO: a proper UrlEncode
                    $.get("/haskell-eval.json?jsonrpc=2.0&id=1&method=eval&params="
                          + JSON.stringify({expr:line.replace(/\+/g,'%2b').replace(/\#/g,'%23')}),
                          function(resp){
                              var result = JSON.parse(resp).result;
                              if (pageTrigger > -1) { triggerTutorialPage(pageTrigger,result); }
                              if (result.type) {
                                  handleSuccess(report,result);
                              } else if (result.error) {
                                  report(
                                      [{msg:result.error,
                                        className:"jquery-console-message-error"}]
                                  );
                              } else if (result.exception) {
                                  report(
                                      [{msg:result.exception,
                                        className:"jquery-console-message-error"}]
                                  );
                              } else if (result.internal) {
                                  report(
                                      [{msg:result.internal,
                                        className:"jquery-console-message-error"}]
                                  );
                              } else if (result.result) {
                                  if (result.expr.match(/^:modules/)) {
                                      report(
                                          [{msg:result.result.replace(/[\["\]]/g,'').replace(/,/g,', '),
                                            className:"jquery-console-message-type"}]);
                                  }
                              }
                          });
                },
                autofocus:true,
                promptHistory:true,
                historyPreserveColumn:true,
                welcomeMessage:'Type Haskell expressions in here.'
            });
        },$.browser.msie?0:500);
    });

    String.prototype.trim = function() {
        return this.replace(/^[\t ]*(.*)[\t ]*$/,'$1');
    };

    ////////////////////////////////////////////////////////////////////////
    // Trigger console commands
    function libTrigger(line,report) {
        switch (line.trim()) {
        case 'help': {
            setTutorialPage(undefined,0);
            report();
            pageTrigger = 0;
            return true;
        }
        default: {
            var m = line.trim().match(/^step([0-9]+)/);
            if (m) {
                if ((m[1]*1) <= pages.length) {
                    setTutorialPage(undefined,m[1]-1);
                    report();
                    pageTrigger = m[1]-1;
                    return true;
                }
            }
        }
        };
    };

    ////////////////////////////////////////////////////////////////////////
    // Change the tutorial page

    function setTutorialPage(result,n) {
        if (pages[n]) {
            tutorialGuide.animate({opacity:0,height:0},'fast',function(){
                if (typeof(pages[n].guide) == 'function')
                    tutorialGuide.html(pages[n].guide(result));
                else
                    tutorialGuide.html(pages[n].guide);
                if (true) tutorialGuide.append('<div class="note">Tip: You\'re at step ' + (n+1) + ', type <code>step' + (n+1) + '</code> to return to this step.</div>');
                tutorialGuide.css({height:'auto'});
                tutorialGuide.animate({opacity:1},'fast');
            });
        }
    };

    ////////////////////////////////////////////////////////////////////////
    // Trigger a page according to a result

    function triggerTutorialPage(n,result) {
        n++;
        if (pages[n] && (typeof (pages[n].trigger) == 'function')
            && pages[n].trigger(result)) {
            pageTrigger++; 
            setTutorialPage(result,n);
        }
    };

    ////////////////////////////////////////////////////////////////////////
    // Trigger various libraries after JSONRPC returned
    function handleSuccess(report,result) {
        if (result.type.match(/^Graphics\.Raphael\.Raphael[\r\n ]/)) {
            //runRaphael(result.result);
            report();
        } else {
            if (result.result) {
                report(
                    [{msg:'=> ' + result.result,
                      className:"jquery-console-message-value"},
                     {msg:':: ' + result.type,
                      className:"jquery-console-message-type"}]
                );
            } else {
                report(
                    [{msg:':: ' + result.type,
                      className:"jquery-console-message-type"}]
                );
            }
        }
    };

    ////////////////////////////////////////////////////////////////////////
    // Raphael support
/*
    function runRaphael(expr) {
        raphaelPaper.clear();
        $('#raphael').parent().parent().slideDown(function(){
            var exprs = expr.split(/\n/g);
            for (var x in exprs)
                raphaelRunExpr(exprs[x]);
        });
    }
    function raphaelRunExpr(expr) {
        var expr = expr.split(/ /g);
        switch (expr[0]) {
        case 'new': {
            switch (expr[2]) {
            case 'circle': {
                var x = expr[3], y = expr[4], radius = expr[5];
                var circle = raphaelPaper.circle(x*1,y*1,radius*1);
                circle.attr("fill", "#7360a4");
                break;
            }
            }
        }
        }
    }
    */
})(jQuery);
