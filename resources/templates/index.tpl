<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='utf-8'/>
    <title>My IDM masterpiece is called …</title>
    <link rel="stylesheet" type="text/css" href="screen.css"/>
    <link rel="shortcut icon" type="image/png" href="/favicon.png"/>
    <link rel="apple-touch-icon" href="fruit-3.png" />
    <link rel="apple-touch-icon" sizes="72x72" href="fruit-pad.png" />
    <link rel="apple-touch-icon" sizes="114x114" href="fruit-4.png" />
    <meta name="viewport" content="width=500, user-scalable=yes" />
  </head>
  <body>
    <div id="vinyl">
      <div id="cover">
        <a href="/"><img src="$(photo-url)" alt=""/></a>
      </div>
    </div>
    <h2><track-name/></h2>

    <div id="footer">
      This is an <a href="https://gitorious.org/flitwemmmmm">open source</a>
      toy which generates plausible IDM track names using a Markov chain.<br/>
      The cover photo is <a href="$(photo-page-url)"><i><photo-title/></i></a>
      by <a href="http://www.flickr.com/people/GlitchBot">GlitchBot</a>.<br/><br/>
      Refresh the page to generate a new track.<br/><br/>
      Made by <a href="http://wjt.me.uk/">wjt</a>; inspired by <a
      href="http://ourbandiscalled.com">Our Band Is Called</a>.
    </div>
  </body>
</html>
