require 'digest'
require "jekyll-less"

# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(cmd)
      cache  = ".cache/runhaskell"
      system "mkdir -p #{cache}"

      digest = Digest::MD5.hexdigest(cmd)
      file   = File.join cache, digest

      if File.exists? file
        puts "#{digest} (#{cmd}) (cached)"
        File.read file
      else
        puts "#{digest} (#{cmd})"
        r = runhaskell_ cmd
        File.write file, r
        r
      end
    end

    def runhaskell_(cmd)
      `runhaskell -i../src #{cmd}`
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)

module Hspec
  class ExtendedExampleTag < Liquid::Tag
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

      # It is crucial to indent nested HTML tags, otherwise a bug in sundowns
      # parser is triggered, which leads to invalid HTML!  See
      # https://github.com/vmg/sundown/issues/139.
      <<-HTML
<div>
  <h4 class="foldable">show example code</h4>
  <div>
  {% highlight hspec %}
  -- file Spec.hs
  #{source}
  {% endhighlight %}
  <pre>
  <code>$ runhaskell Spec.hs</code>
  <samp>{{ "#{file} --html" | runhaskell }}</samp></pre>
  </div>
</div>
      HTML
    end
  end
end

Liquid::Template.register_tag('extended_example', Hspec::ExtendedExampleTag)
