<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='utf-8'/>
    <title>My new single is …</title>
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
        <a href="/"><img src="$(photo-url)" alt="$(photo-title)"/></a>
      </div>
    </div>
    <h2><track-name/></h2>

    <div id="footer">
      This is an <a href="https://github.com/wjt/flitwemmmmm">open source</a>
      toy which generates plausible <a
      href="http://en.wikipedia.org/wiki/Intelligent_dance_music">IDM</a> track
      names using a poorly-implemented Markov chain. Refresh the page to
      generate another.<br/><br/>

      The cover photo is <a href="$(photo-page-url)"><i><photo-title/></i></a>
      by <a href="http://www.flickr.com/people/GlitchBot">GlitchBot</a>.<br/><br/>

      Made by <a href="http://wjt.me.uk/">wjt</a>; powered by <a
      href="http://snapframework.com/">Snap</a> and <a
      href="http://flickr.com/">Flickr</a>; inspired by, and design stolen
      from, <a href="http://ourbandiscalled.com">Our Band Is Called</a> by
      Alexandre Testu and Kevin Bongart; fueled by a deep
      admiration for <a href="http://last.fm/music/Autechre">Autechre</a>, <a
      href="http://www.last.fm/music/Three+Trapped+Tigers">Three Trapped
      Tigers</a>, and all the other bands whose track names formed the source
      material.
    </div>
  </body>
</html>
