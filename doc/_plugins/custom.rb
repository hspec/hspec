
# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(input)
      `runhaskell -i../src #{input}`
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)
