using UnityEngine;

public abstract class MonoSingleton<T> : MonoBehaviour where T : MonoBehaviour
{
    static T mInstance;
    static GameObject BootObject => GameObjectSingle.GetGameObject("Boot");

    public static T Instance
    {
        get
        {
            if (mInstance == null)
            {
                mInstance = FindObjectOfType(typeof(T)) as T;
                if (mInstance == null)
                {
                    var obj = BootObject;
                    if (obj != null)
                    {
                        mInstance = obj.GetComponent<T>() ?? obj.AddComponent<T>();
                    }
                }
            }

            return mInstance;
        }
    }

    void Awake()
    {
        Init();
    }

    /*
     * 没有任何实现的函数，用于保证MonoSingleton在使用前已创建
     */
    public void Startup()
    {
    }

    protected virtual void Init()
    {
    }

    public void DestroySelf()
    {
        Dispose();
        if (mInstance != null)
        {
            Destroy(mInstance);
            mInstance = null;
        }
    }

    public virtual void Dispose()
    {
    }
}