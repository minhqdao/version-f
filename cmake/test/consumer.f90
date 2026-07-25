program consumer
  use version_f, only: version_t

  implicit none

  type(version_t) :: version

  version = version_t('1.2.3')
  if (version%to_string() /= '1.2.3') error stop 'version-f consumer test failed'
end
