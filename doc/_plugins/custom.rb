require 'digest'
require "jekyll-less"

module Hspec
  module CustomFilters
    def runhaskell(args)
      cmd = "runhaskell -Wall -Werror #{args}"
      cache  = "_cache/runhaskell"
      system "mkdir -p #{cache}"

      source = args.split.select {|i| i[/\.hs$/] }.first
      digest = Digest::MD5.hexdigest(cmd + File.read(source))
      file   = File.join cache, digest

      if File.exists? file
        File.read file
      else
        puts "#{cmd}"
        r = `#{cmd}`
          .gsub(/Finished in \S+ seconds/, 'Finished in 0.0005 seconds')
          .gsub(/_includes\/introduction\/MathSpec.hs:/, 'MathSpec.hs:')
          .gsub(/_includes\/introduction\/step2\/Math.hs:/, 'Math.hs:')
          .gsub(/_includes\/[a-zA-Z]+\.hs:/, 'Spec.hs:')
        File.write file, r
        puts "  created cache file #{file}"
        r
      end
    end

    def id(name)
      haskell_identifiers = {
        'property'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#v:property',
        'Property'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t:Property',
        'Testable'          => 'http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/Test-QuickCheck.html#t:Testable',

        '@?='               => 'http://hackage.haskell.org/packages/archive/HUnit/latest/doc/html/Test-HUnit-Base.html#v:-64--63--61-',

        'Spec'              => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html#t:Spec',
        'hspec'             => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec.html#v:hspec',

        'Test.Hspec.QuickCheck' => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-QuickCheck.html',
        'prop'                  => 'http://hackage.haskell.org/packages/archive/hspec/latest/doc/html/Test-Hspec-QuickCheck.html#v:prop',

        'fromHUnitTest'     => 'http://hackage.haskell.org/packages/archive/hspec-contrib/latest/doc/html/Test-Hspec-Contrib-HUnit.html#v:fromHUnitTest',

        'Selector'          => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#t:Selector',
        'shouldThrow'       => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldThrow',
        'errorCall'         => 'http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:errorCall',

        'isPermissionError' => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Error.html#v:isPermissionError',
        'evaluate'          => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#v:evaluate',
        'ErrorCall'         => 'http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#t:ErrorCall',

        'force'             => 'http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html#v:force',
      }
      url = haskell_identifiers[name]
      if url
        "[`#{name}`](#{url})"
      else
        puts "WARNING: No link destination for #{name}!"
        "`#{name}`"
      end
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)

module Hspec
  class NoteTag < Liquid::Tag
    def initialize(tag_name, note, tokens)
      super
      @note = note.strip
    end

    def render(context)
      renderer = Redcarpet::Render::HTML.new
      note = Redcarpet::Markdown.new(renderer).render("**Note:** " + @note)
      "<div class=\"note\">#{note}</div>"
    end
  end

  class RequireTag < NoteTag
    def initialize(tag_name, version, tokens)
      super(tag_name, "This section assumes that you are using `hspec-#{version.strip}` or later.", tokens)
    end
  end
end

Liquid::Template.register_tag('require', Hspec::RequireTag)
Liquid::Template.register_tag('note', Hspec::NoteTag)

module Hspec
  class ExampleTag < Liquid::Tag
    def initialize(tag_name, file, tokens)
      super
      @file = file.strip
    end

    def render(context)
      file = File.join '_includes', @file
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
<pre><kbd class="shell-input">runhaskell Spec.hs</kbd>
<samp>{{ "#{file} --html --seed 921447365 --ignore-dot-hspec" | runhaskell }}</samp></pre>
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
