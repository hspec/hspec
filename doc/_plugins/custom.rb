require 'digest'
require "jekyll-less"

# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(args)
      cmd = "runhaskell -i../src #{args}"
      cache  = ".cache/runhaskell"
      system "mkdir -p #{cache}"

      digest = Digest::MD5.hexdigest(cmd)
      file   = File.join cache, digest

      puts "#{cmd}"
      if File.exists? file
        puts "  using cache file #{file}"
        File.read file
      else
        r = `#{cmd}`
        File.write file, r
        puts "  created cache file #{file}"
        r
      end
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)

module Hspec
  class ExampleTag < Liquid::Tag
    def initialize(tag_name, file, tokens)
      super
      @file = file.strip
    end

    def render(context)
      file = File.join context.registers[:site].source, '_includes', @file
      partial = Liquid::Template.parse(add_wrapping file)
      context.stack do
        partial.render(context)
      end
    end

    def add_wrapping(file)
      source = File.read(file)
<<-HTML
{% highlight hspec %}
-- file Spec.hs
#{source}
{% endhighlight %}
<pre>$ <kbd>runhaskell Spec.hs</kbd>
<samp>{{ "#{file} --html" | runhaskell }}</samp></pre>
HTML
    end
  end
end

module Hspec
  class FoldableExampleTag < ExampleTag
    def add_wrapping(*)
      source = super
      # It is crucial to indent nested HTML tags, otherwise a bug in sundowns
      # parser is triggered, which leads to invalid HTML!  See
      # https://github.com/vmg/sundown/issues/139.
<<-HTML
<div>
  <h5 class="foldable">Example code:</h5>
  <div>
#{source}
  </div>
</div>
HTML
    end
  end
end

Liquid::Template.register_tag('inline_example', Hspec::ExampleTag)
Liquid::Template.register_tag('example', Hspec::FoldableExampleTag)
