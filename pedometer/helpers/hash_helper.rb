class Hash
  def symbolize_keys
    self.inject({}){|h,(k,v)| h[k.to_sym] = v; h}
  end
end