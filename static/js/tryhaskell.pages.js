// Module for the guide pages
tryhaskell.pages = {};

// Unshow a string
tryhaskell.pages.unString = function(str){
    return str.replace(/^"(.*)"$/,"$1").replace(/\\"/,'"');
}

// Random message from a list of messages
tryhaskell.pages.rmsg = function(choices) {
    return choices[Math.floor((Math.random()*100) % choices.length)];
}

// Simple HTML encoding
tryhaskell.pages.htmlEncode = function(text,shy){
    var x = $('<div></div>');
    x.text(text);
    return x.html();
}

// The nemesis
tryhaskell.nemesis = "chirs";

// All pages
tryhaskell.pages.list =
    [
        {title:'Got five minutes?',
         guide:
         '<div class="indent">' +
         '<h3>Got 5 minutes?</h3>' +
         '<p>Type <code title="Click me to insert &quot;help&quot; into the console." style="cursor: pointer;">help</code> to start the ' +
         'tutorial.</p>' +
         '<p>Or try typing these out and see what happens ' +
         '<small class="note">(click to insert)</small>:</p>' +
         '<p>' +
         '<code title="Click me to insert &quot;23 * 36&quot; into the console." style="cursor: pointer;">23 * 36</code> or <code title="Click me to insert &quot;reverse ' +
         '&quot;hello&quot;&quot; into the console." style="cursor: pointer;">reverse ' +
         '"hello"</code> or <code title="Click me to insert &quot;foldr (:) [] [1,2,3]&quot; into the console." style="cursor: pointer;">foldr (:) [] [1,2,3]</code> or <code title="Click me to insert." style="cursor: pointer;">do line <- getLine; putStrLn line</code> or <code>readFile "/welcome"</code>' +
         '</p>' +
         '<p><a href="https://hackage.haskell.org/package/pure-io-0.2.0/docs/PureIO.html#g:2">These</a> IO actions are supported in this app.</p>' +
         '<p><strong>Other cool learning places</strong></pa>' +
         '<p class="clearfix">' +
         'You can also run real Haskell code—file I/O, ' +
         'web apps, and lots more in <a href="https://www.fpcomplete.com/page/project-build">an online IDE</a>. See also the <a href="https://www.fpcomplete.com/school">school of Haskell</a> for practical tutorials.' +
         '</p>' +
         '</div>' +
         '</div>'
        },
        ////////////////////////////////////////////////////////////////////////
        // Lesson 1

        // Simple addition
        {lesson:1,
         title:'Basics; numbers, strings, etc.',
         guide:
         '<h3>' + tryhaskell.pages.rmsg(['Learning By Numbers','Music is Math','Back to Basics'])
         + '</h3>'
         + "<p>To kick off let's try some maths out. Up there you can"
         + " type in Haskell expressions. Try this out: <code>5 + 7</code></p>"
        },
        {guide:function(result){
            if (!result) result = {expr:'5+7',value:12};
            var complied = result.expr.replace(/ /g,'')=="5+7";
            var who = complied? 'we' : 'you';
            return '<h3>' + tryhaskell.pages.rmsg(['Your first Haskell expression',
                                                   "First Time's a Charm"]) + '</h3>'
                + '<p>Well done, you typed it perfect! You got back the number'+
                ' <code>' + result.value + '</code>. Just what '+who+' wanted. '
                + "</p><p>Let's try something completely different."+
                " Type in your name like this:" +
                ' <code>"chris"</code></p>'
        },
         trigger:function(result){
             return result.type.match(/^\(?Num [a-z]+\)? => [a-z]+$/) ||
                 result.type == "Integer" ||
                 result.type == "Int";
         }
        },
        // Strings & types
        {guide:function(result){
            if (!result) result = {expr:'"chris"',value:"\"chris\""};
            var n = tryhaskell.pages.unString(result.value); if (n) n = ", " +n;
            n += "!";
            return '<h3>' + tryhaskell.pages.rmsg(['Types of values',"What's in a name?"]) +
                '</h3>'
                + '<p>Hi there' + tryhaskell.pages.htmlEncode(n)
                + (n!="!"? " That's a pretty name. Honest." : "")
                + " You're getting the hang of this! </p>" +
                // "<p><strong>Note:</strong> You can chat to Haskell programmers while learning here, enter <code>chat</code> to start it."+
                // " You will join the official IRC channel of the Haskell community!</p>"
                "<p>Each time, you're getting back the value of the expression. So "+
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
            if (!result) result = {value:"[42,13,22]"};
            return '<h3>' + tryhaskell.pages.rmsg(["Lesson 1 done already!"]) +
                '</h3>' +
                "<p>Great, you made a list of numbers! If you win we'll split" +
                " the winnings, right?</p>" +
                "<p>Let's see what you've learned so far:</p>" +
                "<ol>"+
                "<li>How to write maths and lists of things.</li>"+
                "</ol>" +
                "<p>You can do stuff with lists. Maybe you want the lottery "+
                "numbers sorted in the right order, try this: " +
                "<code>sort " + result.value + "</code></p>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*\[[0-9, ]+\][ ]*$/) &&
                 result.type.match(/^\(?Num [a-z]+\)? => \[[a-z]+\]$/);
         }
        },
        ////////////////////////////////////////////////////////////////////////
        // Lesson 2 - Functions
        // Functions on lists
        {lesson:2,
         title: 'Simple Functions',
         guide:function(result){
             if (!result) result = {value:"[13,23,30]"};
             return '<h3>' + tryhaskell.pages.rmsg(["We put the funk in function"]) +
                 '</h3>' +
                 "<p>Congratulations, you just used a <strong>function</strong>."+
                 " They're how you get things done in Haskell." +
                 "<p>As you might've guessed, we got back <code>" +
                 tryhaskell.pages.htmlEncode(result.value)
                 + "</code>.</p><p>Ever wanted an evil twin nemesis? Me too. "+
                 "Luckily, you can sort lists of characters, or "+
                 "<strong>strings</strong>" +
                 ", in the same way as numbers! <code>sort \"chris\"</code></p>"
         },
         trigger:function(result){
             return result.expr.match(/sort/) &&
                 result.type.match(/\(?Num [a-z]+, Ord [a-z]+\)? => \[[a-z]+\]$/);
         }
        },
        // Tuples
        {guide:function(result){
            if (!result) result = {value:"\"chirs\""};
            tryhaskell.nemesis = tryhaskell.pages.htmlEncode(tryhaskell.pages.unString(result.value));
            return '<h3>' +
                tryhaskell.pages.rmsg(["Tuples, because sometimes one value ain't enough!"]) +
                '</h3>' +
                "<p>Watch out for "+tryhaskell.nemesis+"! You should keep their credentials for the police.</p>" +
                "<p>My nemesis is 28 years of age: "+
                "<code>(28,\"chirs\")</code></p>"
        },
         trigger:function(result){
             return result.expr.match(/sort/) &&
                 result.type == "[Char]";
         }
        },
        // Functions on tuples
        {guide:function(result){
            if (!result) result = {value:"(28,\"chirs\")"};
            var age = result.value.match(/^\(([0-9]+)+/);
            var villain = tryhaskell.pages.htmlEncode(result.value.replace(/\\"/g,'"'));
            return '<h3>' +
                tryhaskell.pages.rmsg(["We'll keep them safe, don't worry about it."]) +
                '</h3>' +
                "<p>Is "+(age?age[1]:"that")+" a normal age for a " +
                "super-villain?</p>" +
                "<p>You just wrote a <em>tuple</em>. It's a way to keep a bunch of values together in Haskell. " +
                "You can put as many as you like in there:</p>" +
                "<ul><li><code>(1,\"hats\",23/35)</code></li><li><code>(\"Shaggy\",\"Daphnie\",\"Velma\")</code></li></ul>" +
                "<p>Actually, let's say our villain <em>is</em> " +
                "<code>" + villain + "</code>" +
                ", how do you get their age?</p>" +
                "<code>fst " + villain + "</code>"
        },
         trigger:function(result){
             return result.expr.match(/\([0-9]+,[ ]*"[^"]+"\)/) &&
                 result.type.match(/\(?Num [a-z]\)? => \([a-z], \[Char\]\)$/);
         }
        },
        // Summary of lesson 2
        {guide:function(result){
            return '<h3>' +
                tryhaskell.pages.rmsg(["Lesson 2 done! Wow, great job!",
                                       "Lesson 2 completo!"]) +
                '</h3>' +

            "<p>Good job! You got the age back from the tuple! Didn't " +
                " even break a sweat, did you? The <code>fst</code> function "+
                "just gets the <em>first</em> value. It's called \"fst\" because " +
                "it's used <em>a lot</em> in Haskell so it really needs to be short!</p>" +

            "<p>Time to take a rest and see what you learned:</p>" +
                "<ol>"+
                "<li>Functions can be used on lists of any type.</li>" +
                "<li>We can stuff values into tuples.</li>" +
                "<li>Getting the values back from tuples is easy.</li>"+
                "</ol>" +

            "<p>Now let's say you want " +
                " to use a value more than once, how would you do it? "+
                "To make our lives easier, we can say:</p>" +

            "<code>let x = 4 in x * x</code>"
        },
         trigger:function(result){
             return result.expr.match(/fst/) &&
                 result.type.match(/^\(?Num [a-z]\)? => [a-z]$/);
         }
        },
        {guide:function(result){
            return "<h3>Let them eat cake</h3>" +

            "<p>You just <em>bound</em> a <em>variable</em>. " +
                "That is, you bound <code>x</code> to the expression <code>4</code>, " +
                " and then you can write <code>x</code> in some code (the <em>body</em>) and " +
                " it will mean the same as if you'd written <code>4</code>.</p>" +

            "<p>It's like this: <code>let <em>var</em> = <em>expression</em> in <em>body</em></code></p>" +

            "The <code>in</code> part just separates the expression from the body.</p>" +

            "<p>For example try: " +
                "<code><span class='highlight'>let</span> x <span class='highlight'>=</span> 8 * 10 <span class='highlight'>in</span> x + x</code></p>" +

            "<p>So if we wanted to get the age of our villain, we could do:</p>" +

            "<code><span class='highlight'>let</span> villain <span class='highlight'>=</span> (28,\"chirs\") <span class='highlight'>in</span> fst villain</code>"

        },trigger:function(result){
            return result.expr.match(/^[ ]*let[ ]+x[ ]*=[ ]*[0-9]+[ ]*in[ ]*x[ ]*\*[ ]*x/) &&
                result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
        }
        },
        {guide:function(result){
            return "<h3>Basics over, let's go!</h3>" +
                "<p>Next, let's take a short detour to learn about " +
                "<strong>syntactic sugar</strong>. " +
                "Try typing this out:</p>" +
                "<p><code>'a' : []</code></p>" +
                "<p>Or skip to <code>lesson4</code> to learn about functions," +
                " the meat of Haskell!";
        },trigger:function(result){
            return result.expr.match(/^[ ]*let[ ]+villain[ ]*=[ ]*\([0-9]+,[ ]*"[^"]+"\)[ ]*in[ ]+fst[ ]+villain[ ]*/) &&
                result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
        }
        },
        // Lesson 3: Syntactic sugar
        {lesson:3,
         title:'Syntactic Sugar',
         guide:function(result){
             return '<h3>' +
                 tryhaskell.pages.rmsg(["You constructed a list!"]) +
                 '</h3>' +
                 "<p>Well done, that was tricky syntax. You used the <code>(:)</code> " +
                 "function. It takes two values, some value and a list, and " +
                 " constructs a new list" +
                 " out of them. We call it 'cons' for short.</p>" +
                 "<p><code>'a'</code> is " +
                 "the character 'a', <code>[]</code> is an empty list. So " +
                 "tacking <code>'a'</code> at the start of an empty list just "+
                 "makes a list <code>['a']</code>!</p>" +
                 "<p>But thankfully we don't have to type out " +
                 "<code>'a' : 'b' : []</code> every time we want to make a "+
                 "list of characters; we can use " +
                 "<strong>syntactic sugar</strong> and just write"+
                 " <code>['a','b']</code>. Don't believe me, check this!</p>" +
                 "<code>'a' : 'b' : [] == ['a','b']</code>"
         },
         trigger:function(result){
             return result.expr.match(/^[ ]*'a'[ ]*:[ ]*\[\][ ]*/) &&
                 result.type == "[Char]";
         }
        },
        // Booleans and string syntactic sugar
        {guide:function(result){
            return '<h3>' +
                tryhaskell.pages.rmsg(["You're on fire!"]) +
                '</h3>' +
                "<p>You're handling this syntax really well, nice!</p>" +
                "<p>You just got a boolean value back, and it said " +
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
                tryhaskell.pages.rmsg(["Lesson 3 over! Syntactic sugar is sweet"]) +
                '</h3>' +
                "<p>Let's have a gander at what you learned:</p>" +
                "<ol>" +
                "<li>In <code>'a' : []</code>, <code>:</code> is really just " +
                " another function, just clever looking.</li>" +
                "<li>Pretty functions like this are written like <code>(:)</code> when " +
                " you talk about them.</li>" +
                "<li>A list of characters <code>['a','b']</code> can just be written " +
                "<code>\"ab\"</code>. Much easier!</li>"
                + "</ol>" +
                "<p>Phew! You're getting pretty deep! Your arch nemesis, " +
                tryhaskell.nemesis + ", is gonna try to steal your " + tryhaskell.pages.rmsg(['mojo',
                                                                                   'pizza']) +
                "! Let's learn a bit more about functions and passing " +
                "them around. Try this:</p> <code>map (+1) [1..5]</code></p>";
        },
         trigger:function(result){
             return result.expr.replace(/[^\]\[',=\"]?/g,'') == "['','','']==\"\"" &&
                 result.type == "Bool";
         }
        },
        {lesson:4,
         title:'Functions, reloaded; passing, defining, etc.',
         guide:function(){
             var title =
                 tryhaskell.pages.rmsg(["Functions [of a Geisha]",
                                        "Functions, functors, functoids, funky",
                                        "Functions: Expanded fo' real"]);
             return "<h3>" + title + "</h3>" +

             "<p>Here's where the magic begins!</p>" +

             "<p>You just passed the <code>(+1)</code> " +
                 "function to the <code>map</code> function.</p>" +

             "<p>You can try other things like <small class='note'>(remember: click to insert them)</small>:</p>" +

             "<ul>" +
                 "<li><code>map (*99) [1..10]</code></li>" +
                 "<li><code>map (/5) [13,24,52,42]</code></li>" +
                 "<li><code>filter (>5) [62,3,25,7,1,9]</code></li>" +
                 "</ul>" +

             "<p>Note that a tuple is different to a list because you can do this:</p>" +
                 "<code>(1,\"George\")</code>"
         },
         trigger:function(result){
             return result.expr.match(/^[ ]*map[ ]+\(\+1\)[ ]*\[1..5\][ ]*$/) &&
                 (result.type.match(/^\(?Num [a-z], Enum [a-z]\)? => \[[a-z]\]$/) ||
                  result.type.match(/^\(?Enum [a-z], Num [a-z]\)? => \[[a-z]\]$/));
         }},
        {guide:function(result){
            return "<h3>Lists and Tuples</h3>" +

            "<p>You can only " +
                " have a list of numbers or a list of characters, whereas in a tuple you can throw anything in! </p>" +

            "<p>We've also seen that you can make a new list with <code>(:)</code> that joins two values together, like: </p>" +
                "<p><code>1 : [2,3]</code></p>" +

            "<p>But we can't do this with tuples! You can only write a tuple and then look at what's inside. You can't make new ones on the fly like a list." +

            "<p>Let's write our own functions! It's really easy. How about something simple:</p>" +
                "<code>let square x = x * x in square "+tryhaskell.pages.rmsg([52,10,3])+"</code>"

        },
         trigger:function(result){
             return result.expr.match(/^[ ]*\(1,"[^"]+"\)[ ]*$/) &&
                 result.type.match(/^\(?Num [a-z]\)? => \([a-z], \[Char\]\)$/);
         }},
        {guide:function(result){
            return "<h3>Let there be functions</h3>" +
                "<p>Nice one! I think you're getting used to the <code>let</code> syntax.</p>" +
                "<p>You defined a function. You can read it as, as for a given " +
                "<em>parameter</em> called <code>x</code>, <code>square</code> of " +
                "<code>x</code> is <code>x * x</code>." +
                "<p>Some others you can try are:</p>" +
                "<ul><li><code>let add1 x = x + 1 in add1 5</code></li>" +
                "<li><code>let second x = snd x in second (3,4)</code></li>" +
                "</ul>" +
                "<p>Let's go crazy and use our <code>square</code> function with map:</p>" +
                "<code>let square x = x * x in map square [1..10]</code>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]*square[ ]+x[ ]*=[ ]*x[ ]*\*[ ]*x[ ]*in[ ]*square[ ]+[0-9]+/) &&
                 result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
         }},
        {guide:function(result){
            if (!result || !result.value) result = { value: "[1,4,9,16,25,36,49,64,81,100]" };
            return "<h3>Let there be functions</h3>" +

            "<p>That's so cool! You described a simple function <code>square</code> and then " +
                "you just passed it to another function (<code>map</code>) and got back <code>" +
                tryhaskell.pages.htmlEncode(result.value) + "</code>, exactly what you expected!</p>" +

            "<p>Haskell is pretty good at composing things together like this. " +
                "Some other things you can try are:</p>" +

            "<ul>" +
                "<li><code>let add1 x = x + 1 in map add1 [1,5,7]</code></li>" +
                "<li><code>let take5s = filter (==5) in take5s [1,5,2,5,3,5]</code></li>" +
                "<li><code>let take5s = filter (==5) in map take5s [[1,5],[5],[1,1]]</code></li>" +
                "</ul>" +

            "<p>Did you get back what you expected?</p>" +

            "<p>One more example for text; how do you upcase a letter?</p>" +

            "<p><code>toUpper 'a'</code></p>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]+square[ ]+x[ ]*=[ ]*x[ ]*\*[ ]*x[ ]*in[ ]+map[ ]+square[ ]*\[1..10\][ ]*$/) &&
                 (result.type.match(/^\(?Num [a-z], Enum [a-z]\)? => \[[a-z]\]$/) ||
                  result.type.match(/^\(?Enum [a-z], Num [a-z]\)? => \[[a-z]\]$/));
         }},
        {guide:function(result){
            return "<h3>Exercise time!</h3>" +

            "<p>Easy! Remember: characters are written like <code>'a'</code> and " +
                "strings (lists of characters) are written like <code>\"a\"</code>." +

            "<p>I need you to use <code>toUpper</code> capitalise my whole name, " +
                "<code>\"Chris\"</code>. Give it a try." +
                " You can do it, I believe in you!</p>" +

            '<p>Spoiler: <code class="spoiler">map toUpper "Chris"</code></p>'
        },
         trigger:function(result){
             return result.expr.match(/^toUpper 'a'$/) &&
                 result.type == "Char";
         }},
        {guide:function(result){
            return "<h3>Lesson 4 complete!</h3>" +

            "<p>Brilliant! You're making excellent progress! " +
                "You just passed <code>toUpper</code> to <code>map</code>. No problem.</p>" +

            "<p>Let's go over what you've learned in this lesson:</p>" +

            "<ol>" +
                "<li>Functions like <code>map</code> take other functions as parameters.</li>" +
                "<li>Functions like <code>(+1)</code>, <code>(>5)</code> and "+
                "<code>square</code> can be passed to other functions.</li>" +
                "<li>Defining functions is just a case of writing what "+
                "to do with the parameters.</li>"  + "</ol>" +

            "<p>Let's check out <em>pattern matching</em>; a way to "+
                "get values from other values using patterns. Try this: </p>" +
                "<p><code>let (a,b) = (10,12) in a * 2</code></p>"
        },
         trigger:function(result){
             return result.type == "[Char]" &&
                 result.expr.match(/^map[ ]+toUpper/);
         }},
        {lesson:5,
         title:'Pattern Matching',
         guide:function(result){
             var title =
                 tryhaskell.pages.rmsg(["And therefore, patterns emerge in nature.",
                                        "And Then Patterns",
                                        "Pattern matching!"])
             return "<h3>" + title + "</h3>" +

             "<p>Jolly good show!</p>" +
                 "<p>So you had a value <code>(10,12)</code> and matched " +
                 "it against a pattern <code>(a,b)</code>, then you were able" +
                 " to do stuff with the <code>a</code> and <code>b</code>!" +

             "<p>Note: Pattern matching <code>(a,b)</code> against "+
                 "<code>(1,2)</code> to get the <code>a</code> is the same as" +
                 " doing <code>fst (1,2)</code>, like you did in <code>step7</code>!</p>" +

             "<p>A pattern always matches the way the "+
                 "value was originally constructed. Remember that <code>\"abc\"</code> is " +
                 "syntactic sugar for <code>'a' : 'b' : 'c' : []</code>.</p>" +

             "<p>So you can get the characters from a string with patterns:</p>" +

             "<code>let (a:b:c:[]) = \"xyz\" in a</code>"
         },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]+\(a,b\)[ ]+=[ ]+\(10,12\)[ ]+in[ ]+a[ ]*\*[ ]*2[ ]*$/) &&
                 result.type.match(/\(?Num [a-z]\)? => [a-z]$/);
         }},
        {guide:function(result){
            return "<h3>"+tryhaskell.pages.rmsg(["Ignorance is bliss","Ignoring values"])+"</h3>" +

            "<p>You're getting into tricky syntax, huh? I know you can handle it!</p>" +

            "<p>If you just want some of the values, you can ignore the others with <code>_</code> (underscore) like this:</p>" +

            "<p><code>let (a:_:_:_) = \"xyz\" in a</code></p>" +

            "<p>In fact, <code>(a:b:c:d)</code> is short-hand for " +
                "<code>(a:(b:(c:d)))</code>, so you can just ignore the rest in one go:</p>" +

            "<code>let (a:_) = \"xyz\" in a</code>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]+\(a:b:c:\[\]\)[ ]*=[ ]*\"xyz\"[ ]*in[ ]+a[ ]*$/) &&
                 result.type == "Char";
         }},
        {guide:function(result){
            return "<h3>"+tryhaskell.pages.rmsg(["Exercise!","Show me the money!"])+"</h3>" +

            "<p>Try to get the <code>'a'</code> value from this value using pattern matching:</p>" +
                "<p><code>(10,\"abc\")</code></p>" +

            "<p>Spoiler: <code class='spoiler'>let (_,(a:_)) = (10,\"abc\") in a</code></p>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]*\(a:_\)[ ]*=[ ]*"xyz"[ ]*in[ ]*a[ ]*$/) &&
                 result.type == "Char";
         }},
        {guide:function(result){
            return "<h3>"+tryhaskell.pages.rmsg(["Well done!","Brilliant!","Perfetto!"])+"</h3>" +

            "<p>Wizard! I think you've got pattern-matching down.</p>" +

            "<p>If you're still a bit unsure, here are some other things you can try:</p>" +

            "<ul>" +
                "<li><code>let _:_:c:_ = \"abcd\" in c</code></li>" +
                "<li><code>let [a,b,c] = \"cat\" in (a,b,c)</code></li>" +
                "</ul>" +

            "<p>You can also grab a whole value <em>and</em> pattern match on it (have your cake and eat it too):</p>" +

            "<code>let abc@(a,b,c) = (10,20,30) in (abc,a,b,c)</code>"
        },
         trigger:function(result){
             return result.expr.match(/^[ ]*let[ ]*\(_,\(?a:_\)?\)[ ]*=[ ]*\(10,\"abc\"\)[ ]*in[ ]*a[ ]*$/) &&
                 result.type == "Char";
         }},
        {guide:function(result){
            return "<h3>"+tryhaskell.pages.rmsg(["And that's the end of that chapter"])+"</h3>" +

            "<p>That was easy, right?</p>" +

            "<p>Let's go over what you've learned in this lesson:</p>" +

            "<ol>" +
                "<li>Values are pattern matched, or <em>deconstructed</em>, by writing however they were constructed.</li>" +
                "<li>Patterns let you use the values that you match.</li>" +
                "<li>You can ignore whichever values you want.</li>" +
                "<li>You can pattern match and keep hold of the original value too.</li>" +
                "</ol>" +

            "<p>Okay! That's all for now. It's time to dig into <a href='http://learnyouahaskell.com/starting-out'>a book</a>!</p>"

        },
         trigger:function(result){
             return result.type.match(/Num/)
         }}
    ];
