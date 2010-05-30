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


function encodeHex(str){
    var result = "";
    for (var i=0; i<str.length; i++){
        result += "%" + pad(toHex(str.charCodeAt(i)&0xff),2,'0');
    }
    return result;
}

var handleJSON = function(a){ alert('Unassigned JSONP: ' + a); }

var hexv = {
    "00":0,"01":1,"02":2,"03":3,"04":4,"05":5,"06":6,"07":7,"08":8,"09":9,"0A":10,"0B":11,"0C":12,"0D":13,"0E":14,"0F":15,
    "10":16,"11":17,"12":18,"13":19,"14":20,"15":21,"16":22,"17":23,"18":24,"19":25,"1A":26,"1B":27,"1C":28,"1D":29,"1E":30,"1F":31,
    "20":32,"21":33,"22":34,"23":35,"24":36,"25":37,"26":38,"27":39,"28":40,"29":41,"2A":42,"2B":43,"2C":44,"2D":45,"2E":46,"2F":47,
    "30":48,"31":49,"32":50,"33":51,"34":52,"35":53,"36":54,"37":55,"38":56,"39":57,"3A":58,"3B":59,"3C":60,"3D":61,"3E":62,"3F":63,
    "40":64,"41":65,"42":66,"43":67,"44":68,"45":69,"46":70,"47":71,"48":72,"49":73,"4A":74,"4B":75,"4C":76,"4D":77,"4E":78,"4F":79,
    "50":80,"51":81,"52":82,"53":83,"54":84,"55":85,"56":86,"57":87,"58":88,"59":89,"5A":90,"5B":91,"5C":92,"5D":93,"5E":94,"5F":95,
    "60":96,"61":97,"62":98,"63":99,"64":100,"65":101,"66":102,"67":103,"68":104,"69":105,"6A":106,"6B":107,"6C":108,"6D":109,"6E":110,"6F":111,
    "70":112,"71":113,"72":114,"73":115,"74":116,"75":117,"76":118,"77":119,"78":120,"79":121,"7A":122,"7B":123,"7C":124,"7D":125,"7E":126,"7F":127,
    "80":128,"81":129,"82":130,"83":131,"84":132,"85":133,"86":134,"87":135,"88":136,"89":137,"8A":138,"8B":139,"8C":140,"8D":141,"8E":142,"8F":143,
    "90":144,"91":145,"92":146,"93":147,"94":148,"95":149,"96":150,"97":151,"98":152,"99":153,"9A":154,"9B":155,"9C":156,"9D":157,"9E":158,"9F":159,
    "A0":160,"A1":161,"A2":162,"A3":163,"A4":164,"A5":165,"A6":166,"A7":167,"A8":168,"A9":169,"AA":170,"AB":171,"AC":172,"AD":173,"AE":174,"AF":175,
    "B0":176,"B1":177,"B2":178,"B3":179,"B4":180,"B5":181,"B6":182,"B7":183,"B8":184,"B9":185,"BA":186,"BB":187,"BC":188,"BD":189,"BE":190,"BF":191,
    "C0":192,"C1":193,"C2":194,"C3":195,"C4":196,"C5":197,"C6":198,"C7":199,"C8":200,"C9":201,"CA":202,"CB":203,"CC":204,"CD":205,"CE":206,"CF":207,
    "D0":208,"D1":209,"D2":210,"D3":211,"D4":212,"D5":213,"D6":214,"D7":215,"D8":216,"D9":217,"DA":218,"DB":219,"DC":220,"DD":221,"DE":222,"DF":223,
    "E0":224,"E1":225,"E2":226,"E3":227,"E4":228,"E5":229,"E6":230,"E7":231,"E8":232,"E9":233,"EA":234,"EB":235,"EC":236,"ED":237,"EE":238,"EF":239,
    "F0":240,"F1":241,"F2":242,"F3":243,"F4":244,"F5":245,"F6":246,"F7":247,"F8":248,"F9":249,"FA":250,"FB":251,"FC":252,"FD":253,"FE":254,"FF":255
};

function pad(str, len, pad){
    var result = str;
    for (var i=str.length; i<len; i++){
        result = pad + result;
    }
    return result;
}

var digitArray = new Array('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');

function toHex(n){
    var result = ''
    var start = true;
    for (var i=32; i>0;){
        i-=4;
        var digit = (n>>i) & 0xf;
        if (!start || digit != 0){
            start = false;
            result += digitArray[digit];
        }
    }
    return (result==''?'0':result);
}

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
            {lesson:1,
             guide:
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
            {lesson:2,
             guide:function(result){
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
            {lesson:3,
             guide:function(result){
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
                     "<p><code>'a' : []</code></p>" + 
                     "<p>Or skip to <code>lesson4</code> to learn about functions," +
                     " the meat of Haskell!" 
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
            {lesson:4,
             guide:function(result){
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
    var notices = [];
    var controller; // Console controller

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
            if (confirm("Are you sure you want to reset? " +
                        "You will lose your current state.")) {
                controller.reset();
                tutorialGuide.animate({opacity:0,height:0},'fast',function(){
                    tutorialGuide.html(initalGuide);
                    tutorialGuide.css({height:'auto'});
                    tutorialGuide.animate({opacity:1},'fast');
                });
            }
        });

        $('.load-btn').click(function(){
            alert(encodeHex("a bcd"));
            /*
              $('#editor').focus();
              var line = $('#editor').val();
              // /haskell-eval.json?jsonrpc=2.0&method=load&id=1&params={"contents":"x=1"}
              $.get("/tryhaskell/haskell-eval.json?jsonrpc=2.0&id=1&method=load&params="
              + JSON.stringify({expr:line.replace(/\+/g,'%2b')
              .replace(/\#/g,'%23')}),
              function(resp){
              
              });
            */
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
        var toldAboutRet = false;
        var tellAboutRet;

        function jsonp(url,func) {
            var script = $('<script type="text/javascript" src="'+url+'"></script>');
            handleJSON = function(r){
                script.remove();
                func(r);
            };
            script.attr('src',url);
            $('body').append(script);
        }

        ////////////////////////////////////////////////////////////////////////
        // Create console
        var console = $('.console');
        controller = console.console({
            promptLabel: '> ',
            commandValidate:function(line){
                if (line == "") return false; // Empty line is invalid
                else return true;
            },
            commandHandle:function(line,report){
                if (tellAboutRet) tellAboutRet.fadeOut(function(){
                    $(this).remove();
                });
                if (libTrigger(line,report)) return;
                var ajaxloader = $('<p class="ajax-loader">Loading...</p>');
                controller.inner.append(ajaxloader);
                controller.scrollToBottom();
                // TODO: a proper UrlEncode
                jsonp("http://tryhaskell.org/haskell.json?method=eval&pad=handleJSON&expr=" + encodeHex(line),
                      function(resp){
                          ajaxloader.remove();
                          $('.jquery-console-prompt').each(function(){
                              if (!$(this).hasClass('prompt-done')) {
                                  $(this).addClass('prompt-done');
                                  $(this).click(function(){
                                      controller.promptText($(this).text());
                                  });
                              }
                          });
                          var result = resp;
                          if (pageTrigger > -1) {
                              triggerTutorialPage(pageTrigger,result); }
                          if (result.type) {
                              handleSuccess(report,result);
                          } else if (result.error) {
                              report(
                                  [{msg:result.error,
                                    className:"jquery-console-message-error jquery-console-message-compile-error"}]
                              );
                              notice('compile-error',
                                     "A compile-time error! "+
                                     "It just means the expression wasn't quite right. " +
                                     "Try again.",
                                     'prompt');
                          } else if (result.exception) {
                              var err = limitsError(result.exception);
                              report(
                                  [{msg:err,
                                    className:"jquery-console-message-error jquery-console-message-exception"}]
                              );
                              if (err == result.exception) {
                                  notice('compile-error',
                                         "A run-time error! The expression was right but the"+
                                         " result didn't make sense. Check your expression and try again.",
                                         'prompt');
                              }
                          } else if (result.internal) {
                              report(
                                  [{msg:limitsError(result.internal),
                                    className:"jquery-console-message-error jquery-console-message-internal"}]
                              );
                          } else if (result.result) {
                              if (result.expr.match(/^:modules/)) {
                                  report(
                                      [{msg:result.result.replace(/[\["\]]/g,'')
                                        .replace(/,/g,', '),
                                        className:"jquery-console-message-type"}]);
                              }
                          }
                      });
            },
            charInsertTrigger:function(){
                var t = notice('tellaboutreturn',
                               "Hit Return when you're "+
                               "finished typing your expression.");
                if (t) tellAboutRet = t;
                return true;
            },
            autofocus:true,
            promptHistory:true,
            historyPreserveColumn:true,
            welcomeMessage:'Type Haskell expressions in here.'
        });

        makeGuidSamplesClickable();
    });

    function makeGuidSamplesClickable() {
        $('.guide code').each(function(){
            $(this).css('cursor','pointer');
            $(this).attr('title','Click me to insert "' +
                         $(this).text() + '" into the console.');
            $(this).click(function(){
                controller.promptText($(this).text());
                controller.inner.click();
            });
        });
    }

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
            var m = line.trim().match(/^lesson([0-9]+)/);
            if (m) {
                for (var i = 0; i < pages.length; i++) {
                    if (pages[i].lesson == m[1]*1) {
                        setTutorialPage(undefined,i);
                        report();
                        pageTrigger = i;
                        return true;
                    }
                }
            }
        }
        };
    };

    ////////////////////////////////////////////////////////////////////////
    // Change the tutorial page

    function setTutorialPage(result,n) {
        if (pages[n]) {
            tutorialGuide.find('.lesson').remove();
            tutorialGuide.animate({opacity:0,height:0},'fast',function(){
                if (typeof(pages[n].guide) == 'function')
                    tutorialGuide.html(pages[n].guide(result));
                else
                    tutorialGuide.html(pages[n].guide);
                if (true) tutorialGuide
                    .append('<div class="note">Tip: You\'re at step ' + (n+1)
                            + ', type <code>step' + (n+1)
                            + '</code> to return to this step.</div>')
                    .append('<div class="lesson">Lesson: ' +
                            searchLessonBack(n) +
                            '</div>');
                tutorialGuide.css({height:'auto'});
                tutorialGuide.animate({opacity:1},'fast');
                makeGuidSamplesClickable();
            });
        }
    };

    function searchLessonBack(page) {
        for (var i = page; i >= 0; i--) {
            if (pages[i].lesson) return pages[i].lesson;
        }
        return "1";
    }

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

    function notice(name,msg,style) {
        if (!notices[name]) {
            notices[name] = name;
            return controller.notice(msg,style);
        }
    }

    function limitsError(str) {
        if (str == "Terminated!") {
            notice('terminated',
                   "This error means it took to long to work" +
                   " out on the server.",
                   'fadeout');
            return "Terminated!";
        } else if (str == "Time limit exceeded.") {
            notice('exceeded',
                   "This error means it took to long to work out on the server. " +
                   "Try again.",
                   'fadeout');
            return "Terminated! Try again.";
        }
        return str;
    }

})(jQuery);
