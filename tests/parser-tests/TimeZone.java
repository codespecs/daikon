abstract public class TimeZone implements Serializable ,Cloneable
{
  private static class DisplayNames
  {
    // Cache for managing display names per timezone per locale
    // The structure is:
    //   Map(key=id, value=SoftReference(Map(key=locale, value=displaynames)))
    private static final Map<String,SoftReference<Map<Locale,String[]>>> CACHE
      = new ConcurrentHashMap<String,SoftReference<Map<Locale,String[]>>>() ;
  }

  private static final String[] getDisplayNames(String id, Locale locale)
  {
    Map<String,SoftReference<Map<Locale,String[]>>> displayNames = DisplayNames
      .CACHE;
    SoftReference<Map<Locale,String[]>> ref = displayNames.get(id);
    if ( ref != null )
    {
      Map<Locale,String[]> perLocale = ref.get();
      if ( perLocale != null )
      {
        String[] names = perLocale.get(locale);
        if ( names != null )
        {
          return names;
        }
        names = TimeZoneNameUtility.retrieveDisplayNames(id, locale);
        if ( names != null )
        {
          perLocale.put(locale, names);
        }
        return names;
      }
    }
    String[] names = TimeZoneNameUtility.retrieveDisplayNames(id, locale);
    if ( names != null )
    {
      Map<Locale,String[]> perLocale = new ConcurrentHashMap<Locale,String[]>()
        ;
      perLocale.put(locale, names);
      ref = new SoftReference<Map<Locale,String[]>>(perLocale);
      displayNames.put(id, ref);
    }
    return names;
  }
}
