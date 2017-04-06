require "redcarpet"

module Hspec
  module Toc
    def toc(content)
      renderer = Redcarpet::Render::HTML_TOC.new(nesting_level: 2)
      Redcarpet::Markdown.new(renderer).render(content)
    end
  end
end

Liquid::Template.register_filter(Hspec::Toc)
