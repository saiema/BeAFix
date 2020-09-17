package ar.edu.unrc.dc.mutation.mutantLab.testGeneration;

import edu.mit.csail.sdg.ast.Sig;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class CESigAndFieldOverriding {

    /**
     * The path to a default .properties file
     */
    private static final String OVERRIDING_EXT = ".overrides";
    private static final Path DEFAULT_OVERRIDING_FOLDER = Paths.get("modelOverrides");
    private static final String SIGNATURE_PREFIX = "signature.";
    private static final String FIELD_PREFIX = "field.";
    private static final String FUNCTION_PREFIX = "function.";
    private static final String IGNORE = "ignore";

    /**
     * The {@code StrykerConfig} instance that will be returned by {@link CESigAndFieldOverriding#getInstance()}
     */
    private static CESigAndFieldOverriding instance = null;

    /**
     * @return a previously built instance or construct a new instance using {@code StrykerConfig#DEFAULT_PROPERTIES}
     */
    public static CESigAndFieldOverriding getInstance() {
        if (instance == null) {
            try {
                instance = new CESigAndFieldOverriding();
            } catch (IOException e) {
                throw new IllegalStateException("Exception when trying to load properties");
            }
        }
        return instance;
    }

    private Map<String, Properties> modelOverridings;
    private Path overridesFolder;
    private boolean noOverrides;

    private CESigAndFieldOverriding() throws IOException {
        changeOverridesFolder(DEFAULT_OVERRIDING_FOLDER);
        noOverrides = false;
    }

    public void noOverrides() {
        noOverrides = true;
    }

    public void changeOverridesFolder(String newFolder) throws IOException {
        if (noOverrides)
            return;
        changeOverridesFolder(Paths.get(newFolder));
    }

    public void changeOverridesFolder(Path newFolder) throws IOException {
        if (noOverrides)
            return;
        if (newFolder ==  null)
            throw new IllegalArgumentException("null folder");
        modelOverridings = new HashMap<>();
        if (newFolder.isAbsolute()) {
            overridesFolder = newFolder;
        } else {
            overridesFolder = Paths.get(System.getProperty("user.dir"), newFolder.toString());
        }
        File oFolder = overridesFolder.toFile();
        if (!oFolder.exists()) {
            if (!oFolder.mkdirs())
                throw new IOException("Couldn't create overrides folder " + oFolder.toString());
        } else if (oFolder.isFile()) {
            throw new IOException("Overrides folder is not a folder but a file " + oFolder.toString());
        }
    }

    public void loadPropertiesForModel(String model) throws IOException {
        if (noOverrides)
            return;
        File propFile = createConfigFileIfMissing(Paths.get(overridesFolder.toString(), (model + OVERRIDING_EXT)).toString());
        InputStream inputStream = new FileInputStream(propFile);
        Properties prop = new Properties();
        prop.load(inputStream);
        modelOverridings.put(model, prop);
    }

    public void loadProperties() throws IOException {
        if (noOverrides)
            return;
        File[] filesInOverridesFolder = overridesFolder.toFile().listFiles();
        if (filesInOverridesFolder == null)
            return;
        for (File f : filesInOverridesFolder) {
            if (f.isFile() && f.getName().endsWith(OVERRIDING_EXT)) {
                String model = f.getName().replace(OVERRIDING_EXT, "");
                loadPropertiesForModel(model);
            }
        }
    }

    private File createConfigFileIfMissing(String configFile) throws IOException {
        File propFile = new File(configFile);
        if (!propFile.exists())
            if (!propFile.createNewFile())
                throw new IllegalStateException("Couldn't create new file " + configFile);
        return propFile;
    }

    public boolean signatureOverridePresent(Sig s, String model) {
        if (noOverrides)
            return false;
        return overridePresent(SIGNATURE_PREFIX + clean(s.label), clean(model));
    }

    public boolean fieldOverridePresent(Sig.Field f, String model) {
        if (noOverrides)
            return false;
        return overridePresent(FIELD_PREFIX + clean(f.label), clean(model));
    }

    private boolean overridePresent(String key, String model) {
        if (modelOverridings.containsKey(model)) {
            Properties prop = modelOverridings.get(model);
            if (prop.containsKey(key)) {
                String value = prop.getProperty(key, "");
                return !value.trim().isEmpty();
            }
        }
        return false;
    }

    public boolean signatureShouldBeIgnored(Sig s, String model) {
        if (noOverrides)
            return false;
        return signatureOverridePresent(s, model) && getValueArgument(SIGNATURE_PREFIX + clean(s.label), clean(model)).compareToIgnoreCase(IGNORE) == 0;
    }

    public boolean fieldShouldBeIgnored(Sig.Field f, String model) {
        if (noOverrides)
            return false;
        return fieldOverridePresent(f, model) && getValueArgument(FIELD_PREFIX + clean(f.label), clean(model)).compareToIgnoreCase(IGNORE) == 0;
    }

    public boolean fieldIsOverridenByFunction(Sig.Field f, String model) {
        if (noOverrides)
            return false;
        String value = getValueArgument(FIELD_PREFIX + clean(f.label), clean(model));
        return !value.isEmpty() && value.compareTo(IGNORE) != 0;
    }

    public String getFieldOverridingFunction(Sig.Field f, String model) {
        if (noOverrides)
            return "";
        return getValueArgument(FIELD_PREFIX + clean(f.label), clean(model));
    }

    private String getValueArgument(String key, String model) {
        if (modelOverridings.containsKey(model)) {
            String rawValue = modelOverridings.get(model).getProperty(key, "");
            return rawValue.replace(FUNCTION_PREFIX, "");
        }
        return "";
    }

    private String clean(String key) {
        int lastSlash = key.lastIndexOf('/');
        if (lastSlash == -1)
            return key;
        return key.substring(lastSlash + 1);
    }

}
